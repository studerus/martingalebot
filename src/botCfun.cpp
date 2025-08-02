#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

List botCfun(double base_order_volume, double first_safety_order_volume,
             int n_safety_orders, double take_profit, double pricescale,
             double volumescale, double pricemult, double stoploss,
             double trading_fee, bool show_trades, bool plot,
             bool start_asap, bool compound, NumericVector price,
             DatetimeVector date, LogicalVector deal_start) {
  if (base_order_volume <= 0) {
    stop("base_order_volume must be positive.");
  }
  if (first_safety_order_volume <= 0) {
    stop("first_safety_order_volume must be positive.");
  }
  if (n_safety_orders < 0) {
    stop("n_safety_orders cannot be negative.");
  }
  if (take_profit <= 0) {
    stop("take_profit must be positive.");
  }
  if (pricescale <= 0) {
    stop("pricescale must be positive.");
  }
  if (volumescale <= 0) {
    stop("volumescale must be positive.");
  }
  if (stoploss < 0 || stoploss >= 100) {
    stop("stoploss must be between 0 and 100.");
  }

  int n = 20, k = 0, j = 0, cycle = 1, n_trades = 0, n_stoploss = 0;
  int i_end = price.size(), plot_step = i_end / 2000, base_order_buy_time = 0;
  int n_orders = n_safety_orders + 1, time_diff = 0, last_so_buy_time = 0;
  if (plot) {
    n = 50000 + plot_step;
    show_trades = true;
  }
  double profit, capital, required_capital, final_capital, capital_to_invest;
  double highest_capital, lowest_capital, down_tolerance;
  double buy_price = 0, sell_price = R_PosInf, stoploss_price = 0;
  double base_order_price = price[0], current_invested = 0.0, current_bought_coins = 0.0;
  double current_avg_price = 0.0;
  double time_inactive  = 0.0, max_trade_time = 0.0;
  double max_draw_down = 1, current_bottom = R_PosInf;
  double take_profit_factor, fee_factor = (100 - trading_fee) / 100;
  double stoploss_factor;
  if (stoploss == 0) {
    stoploss_factor = 0;
  } else {
    stoploss_factor = ((100 - stoploss) / 100);
  }
  NumericVector dev (n_orders), volume (n_orders), proportion (n_orders);
  NumericVector bought_amount (n_orders), buy_price_factor (n_orders);
  NumericVector bought_price (n_orders), avg_price (n_orders);
  NumericVector required_change (n_orders), Price = (n), Average_Buy_Price (n);
  DatetimeVector Time = (n);
  NumericVector Price_Change = (n), Bought = (n), Sold = (n);
  NumericVector Dollar = (n), Cycle = (n), Capital = (n);
  DataFrame trades = DataFrame::create(Named("Time") = Time,
      Named("Price") = Price, Named("Average_Buy_Price") = Average_Buy_Price,
      Named("Price_Change") = Price_Change, Named("Bought") = Bought,
      Named("Sold") = Sold, Named("Dollar") = Dollar, Named("Cycle") = Cycle,
      Named("Capital") = Capital);
  volume[0] = base_order_volume;
  volume[1] = first_safety_order_volume;
  dev[1] = pricescale;
  for (int i = 2; i < n_orders; ++i) {
    volume[i] = volume[i - 1] * volumescale;
    dev[i] = dev[i - 1] + (dev[i - 1] - dev[i - 2]) * pricemult;
  }
  down_tolerance = (sum(dev * volume) / sum(volume)) - take_profit;
  buy_price_factor = (100 - dev) / 100;
  take_profit_factor = (100 + take_profit) / 100;
  required_capital = sum(volume);
  highest_capital = required_capital;
  lowest_capital = required_capital;
  capital_to_invest = required_capital;
  proportion = volume / required_capital;
  bought_amount = buy_price_factor * volume;
  NumericVector cum_bought_amount = cumsum(bought_amount);
  NumericVector cum_volume = cumsum(volume);
  NumericVector cum_proportion = cumsum(proportion);
  avg_price = cum_bought_amount / cum_volume;
  required_change = avg_price * take_profit_factor / buy_price_factor;
  capital = required_capital;
  for (int i = 0; i < i_end; ++i) {
    if (price[i] < current_bottom) {
      if (k != 0) {
        current_bottom = price[i];
      }
      if (price[i] <= stoploss_price) {
        lowest_capital = capital + fee_factor * current_bought_coins *
          current_bottom - current_invested;
        if (max_draw_down > (lowest_capital / highest_capital)) {
          max_draw_down = lowest_capital / highest_capital;
        }
        capital += fee_factor * current_bought_coins * price[i] - current_invested;
        if (show_trades & (j < n)) {
          Time[j] = date[i];
          Cycle[j] = cycle;
          Price[j] = price[i];
          Price_Change[j] = 100 * ((Price[j] / Price[j - 1]) - 1);
          Sold[j] = current_invested / current_avg_price;
          Bought[j] = NA_REAL;
          Average_Buy_Price[j] = NA_REAL;
          Dollar[j] = Sold[j] * Price[j];
          Capital[j] = capital;
          j += 1;
        }
        time_diff = date[i] - date[base_order_buy_time];
        if (max_trade_time < time_diff) {
          max_trade_time = time_diff;
        }
        if (last_so_buy_time > 0) {
          time_inactive += date[i] - date[last_so_buy_time];
        }
        n_trades += 1;
        n_stoploss += 1;
        cycle += 1;
        current_invested = 0;
        current_bought_coins = 0;
        k = 0;
        current_bottom = R_PosInf;
        buy_price = 0;
        sell_price = R_PosInf;
        stoploss_price = R_NegInf;
      } else if ((price[i] <= buy_price) | (k == 0)) {
        // Only apply the trend filter to the very first order of a cycle;
        // safety orders (k > 0) should always be allowed
        if ((k == 0 ? (start_asap || deal_start[i]) : true)) {
          if (k == 0) {
            base_order_price = price[i];
            buy_price = base_order_price;
            base_order_buy_time = i;
          }
          sell_price = buy_price * required_change[k];
          if (compound) {
            capital_to_invest = capital;
          }
          current_invested = capital_to_invest * cum_proportion[k];
          current_avg_price = base_order_price * avg_price[k];
          current_bought_coins = fee_factor * current_invested / current_avg_price;
          stoploss_price = (capital * stoploss_factor - capital + current_invested) /
            (fee_factor * current_bought_coins);
          if (show_trades & (j < n)) {
            Time[j] = date[i];
            Cycle[j] = cycle;
            Price[j] = buy_price;
            Dollar[j] = capital_to_invest * proportion[k];
            Bought[j] = fee_factor * Dollar[j] / Price[j];
            Sold[j] = NA_REAL;
            Average_Buy_Price[j] = current_avg_price;
            Price_Change[j] = 100 * ((Price[j] / base_order_price) - 1);
            Capital[j] = capital - current_invested +
              fee_factor * current_bought_coins * Price[j];
            j += 1;
          }
          k += 1;
          if (k < n_orders) {
            buy_price = buy_price_factor[k] * base_order_price;
            last_so_buy_time = 0;
          } else {
            last_so_buy_time = i;
            buy_price = 0;
          }
        }
      }
    } else if (price[i] >= sell_price) {
      lowest_capital = capital + fee_factor * current_bought_coins *
        current_bottom - current_invested;
      if (max_draw_down > (lowest_capital / highest_capital)) {
        max_draw_down = lowest_capital / highest_capital;
      }
      capital += fee_factor * current_bought_coins *
        sell_price - current_invested;
      if (capital > highest_capital) {
        highest_capital = capital;
      }
      if (show_trades & (j < n)) {
        Time[j] = date[i];
        Cycle[j] = cycle;
        Price[j] = sell_price;
        Price_Change[j] = 100 * ((Price[j] / Price[j - 1]) - 1);
        Sold[j] = current_invested / current_avg_price;
        Bought[j] = NA_REAL;
        Average_Buy_Price[j] = NA_REAL;
        Dollar[j] = Sold[j] * Price[j];
        Capital[j] = capital;
        j += 1;
      }
      if (highest_capital < capital) {
        highest_capital = capital;
        lowest_capital = capital;
      }
      time_diff = date[i] - date[base_order_buy_time];
      if (max_trade_time < time_diff) {
        max_trade_time = time_diff;
      }
      if (last_so_buy_time > 0) {
        time_inactive += date[i] - date[last_so_buy_time];
      }
      n_trades += 1;
      cycle += 1;
      current_invested = 0;
      current_bought_coins = 0;
      k = 0;
      current_bottom = R_PosInf;
      buy_price = 0;
      sell_price = R_PosInf;
      stoploss_price = R_NegInf;
    }
  }
  if (plot) {
    for (int i = plot_step; i < i_end;) {
      Price[j] = price[i];
      Time[j] = date[i];
      Cycle[j] = NA_REAL;
      Sold[j] = NA_REAL;
      j += 1;
      i += plot_step;
    }
  }

  final_capital = capital + fee_factor * current_bought_coins *
    price[i_end - 1]  - current_invested;

  lowest_capital = capital + fee_factor * current_bought_coins *
    current_bottom - current_invested;
  if (final_capital > highest_capital) {
    highest_capital = capital;
  }
  if (max_draw_down > (lowest_capital / highest_capital)) {
    max_draw_down = lowest_capital / highest_capital;
  }
  max_draw_down = 100 * (1 - max_draw_down);

  time_diff = date[i_end - 1] - date[base_order_buy_time];
  if (max_trade_time < time_diff) {
    max_trade_time = time_diff;
  }
  if (k >= n_orders) {
    time_inactive += date[i_end - 1] - date[last_so_buy_time];
  }

  profit = 100 * ((final_capital / required_capital) - 1);

  time_inactive = 10000 * time_inactive / (date[i_end - 1] - date[0]);
  time_inactive = round(time_inactive) / 100;

  DataFrame df = DataFrame::create(Named("profit") = profit,
                                   Named("n_trades") = n_trades,
                                   Named("max_draw_down") = max_draw_down,
                                   Named("required_capital") = required_capital,
                                   Named("covered_deviation") = dev[n_orders - 1],
                                   Named("down_tolerance") = down_tolerance,
                                   Named("max_time") = max_trade_time / 86400,
                                   Named("percent_inactive") = time_inactive,
                                   Named("n_stoploss") = n_stoploss);
  List L = List::create(df, trades);
  return L;
}
