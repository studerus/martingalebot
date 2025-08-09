#include <Rcpp.h>
#include <algorithm> // for std::max

using namespace Rcpp;

// [[Rcpp::export]]
List botCfun(double base_order_volume, double first_safety_order_volume,
             int n_safety_orders, double take_profit, double pricescale,
             double volumescale, double pricemult, double stoploss,
             bool trailing_take_profit, double trailing_rate,
             double trading_fee, bool show_trades, bool plot,
             bool start_asap, bool compound, bool use_emergency_stop,
             NumericVector price, DatetimeVector date, 
             LogicalVector deal_start, LogicalVector emergency_stop) {

  // --- Parameter Validation ---
  if (base_order_volume <= 0) stop("base_order_volume must be positive.");
  if (first_safety_order_volume <= 0) stop("first_safety_order_volume must be positive.");
  if (n_safety_orders < 0) stop("n_safety_orders cannot be negative.");
  if (take_profit <= 0) stop("take_profit must be positive.");
  if (pricescale <= 0) stop("pricescale must be positive.");
  if (volumescale <= 0) stop("volumescale must be positive.");
  if (stoploss < 0 || stoploss >= 100) stop("stoploss must be between 0 and 100.");

  // --- Initial Setup ---
  int n = 20, j = 0, cycle = 1, n_trades = 0, n_stoploss = 0, n_emergency_stops = 0;
  const int i_end = price.size();
  int plot_step = (i_end > 0) ? std::max(1, i_end / 2000) : 1;
  int n_orders = n_safety_orders + 1, time_diff = 0;

  if (plot) {
    n = 50000 + plot_step;
    show_trades = true;
  }

  double profit, capital, required_capital, final_capital, capital_to_invest;
  double highest_capital, lowest_capital, down_tolerance;
  double time_inactive  = 0.0, max_trade_time = 0.0;
  double max_draw_down = 1.0;

  const double fee_factor = (100.0 - trading_fee) / 100.0;
  const double stoploss_factor = (stoploss == 0) ? 0.0 : ((100.0 - stoploss) / 100.0);
  const bool trailing_enabled = trailing_take_profit;

  // --- Pre-calculate strategy vectors ---
  NumericVector dev(n_orders), volume(n_orders), proportion(n_orders);
  NumericVector buy_price_factor(n_orders), avg_price(n_orders);
  NumericVector required_change(n_orders);

  volume[0] = base_order_volume;
  if (n_safety_orders > 0) {
    volume[1] = first_safety_order_volume;
    dev[1] = pricescale;
    for (int i = 2; i < n_orders; ++i) {
      volume[i] = volume[i - 1] * volumescale;
      dev[i] = dev[i - 1] + (dev[i - 1] - dev[i - 2]) * pricemult;
    }
  }

  // Compute down_tolerance using weighted-average price per coin logic
  // tp_over_start = (1 + take_profit/100) * sum(volume) / sum(volume / (1 - dev/100))
  {
    NumericVector one_minus_dev = (100.0 - dev) / 100.0;
    double sum_volume = sum(volume);
    double denom = sum(volume / one_minus_dev);
    double tp_over_start = ((100.0 + take_profit) / 100.0) * (sum_volume / denom);
    down_tolerance = 100.0 * (1.0 - tp_over_start);
  }
  buy_price_factor = (100.0 - dev) / 100.0;
  const double take_profit_factor = (100.0 + take_profit) / 100.0;

  required_capital = sum(volume);
  highest_capital = required_capital;
  lowest_capital = required_capital;
  capital_to_invest = required_capital;
  proportion = volume / required_capital;
  
  NumericVector cum_proportion = cumsum(proportion);
  NumericVector cum_volume = cumsum(volume);
  // Average buy price factor relative to base order price must be
  // sum(volume) / sum(volume / buy_price_factor) (harmonic weighting),
  // not the arithmetic average of buy_price_factor.
  NumericVector cum_denom = cumsum(volume / buy_price_factor);
  avg_price = cum_volume / cum_denom; // factor relative to base order price
  required_change = avg_price * take_profit_factor;

  // --- DataFrame for trades ---
  NumericVector Price(n), Average_Buy_Price(n), Price_Change(n), Bought(n), Sold(n), Dollar(n), Cycle(n), Capital(n);
  DatetimeVector Time(n);
  CharacterVector Trade_Type(n);

   DataFrame trades = DataFrame::create(Named("Time") = Time,
      Named("Price") = Price, Named("Average_Buy_Price") = Average_Buy_Price,
      Named("Price_Change") = Price_Change, Named("Bought") = Bought,
      Named("Sold") = Sold, Named("Dollar") = Dollar, Named("Cycle") = Cycle,
      Named("Capital") = Capital, Named("Trade_Type") = Trade_Type);
      
  // --- Main Loop ---
  capital = required_capital;
  
  // --- Deal state variables (local for performance) ---
  int k = 0, base_order_buy_time = 0, last_so_buy_time = 0;
  double buy_price = 0.0, sell_price = R_PosInf, stoploss_price = R_NegInf;
  double base_order_price = 0.0, current_invested = 0.0, current_bought_coins = 0.0;
  double current_avg_price = 0.0, current_bottom = R_PosInf;
  bool trailing_active = false;
  double highest_price_since_activation = 0.0;
  
  auto reset_deal_state = [&]() {
      k = 0; base_order_buy_time = 0; last_so_buy_time = 0;
      buy_price = 0.0; sell_price = R_PosInf; stoploss_price = R_NegInf;
      base_order_price = 0.0; current_invested = 0.0; current_bought_coins = 0.0;
      current_avg_price = 0.0; current_bottom = R_PosInf;
      trailing_active = false; highest_price_since_activation = 0.0;
  };

  for (int i = 0; i < i_end; ++i) {
    const double current_price = price[i];
    
    // --- EMERGENCY STOP ---
    if (use_emergency_stop && emergency_stop[i]) {
      if (k > 0) { // Only if a deal is active
        lowest_capital = capital + fee_factor * current_bought_coins * current_bottom - current_invested;
        if (max_draw_down > (lowest_capital / highest_capital)) {
          max_draw_down = lowest_capital / highest_capital;
        }
        capital += fee_factor * current_bought_coins * current_price - current_invested;

        if (show_trades & (j < n)) {
          Time[j] = date[i]; Cycle[j] = cycle; Price[j] = current_price;
          Price_Change[j] = 100 * ((Price[j] / price[i - 1]) - 1);
          Sold[j] = current_invested / current_avg_price;
          Bought[j] = NA_REAL; Average_Buy_Price[j] = NA_REAL;
          Dollar[j] = Sold[j] * Price[j]; Capital[j] = capital;
          Trade_Type[j] = "Emergency Stop";
          j++;
        }
        
        time_diff = date[i] - date[base_order_buy_time];
        if (max_trade_time < time_diff) max_trade_time = time_diff;
        if (last_so_buy_time > 0) time_inactive += date[i] - date[last_so_buy_time];

        n_trades++; 
        n_emergency_stops++; 
        cycle++; 
        reset_deal_state();
      }
      continue; // Skip the rest of the logic for this tick
    }
    
    // --- TRAILING TAKE PROFIT ---
    if (trailing_active) {
      highest_price_since_activation = std::max(highest_price_since_activation, current_price);
      double trail_stop_price = highest_price_since_activation * (1.0 - trailing_rate);

      if (current_price <= trail_stop_price) {
        lowest_capital = capital + fee_factor * current_bought_coins * current_bottom - current_invested;
        if (max_draw_down > (lowest_capital / highest_capital)) max_draw_down = lowest_capital / highest_capital;
        
        capital += fee_factor * current_bought_coins * trail_stop_price - current_invested;
        if (capital > highest_capital) highest_capital = capital;

        if (show_trades & (j < n)) {
            Time[j] = date[i]; Cycle[j] = cycle; Price[j] = trail_stop_price;
            Price_Change[j] = 100 * ((Price[j] / price[i-1]) - 1);
            Sold[j] = current_invested / current_avg_price;
            Bought[j] = NA_REAL; Average_Buy_Price[j] = NA_REAL;
            Dollar[j] = Sold[j] * Price[j]; Capital[j] = capital;
            Trade_Type[j] = "Take Profit";
            j++;
        }
        
        time_diff = date[i] - date[base_order_buy_time];
        if (max_trade_time < time_diff) max_trade_time = time_diff;
        if (last_so_buy_time > 0) time_inactive += date[i] - date[last_so_buy_time];
        
        n_trades++; cycle++; reset_deal_state();
      }
      continue; 
    }

    // --- REGULAR TRADING LOGIC ---
    if (current_price < current_bottom) {
      if (k > 0) current_bottom = current_price;
      
      if (k > 0 && current_price <= stoploss_price) {
        lowest_capital = capital + fee_factor * current_bought_coins * current_bottom - current_invested;
        if (max_draw_down > (lowest_capital / highest_capital)) max_draw_down = lowest_capital / highest_capital;
        
        capital += fee_factor * current_bought_coins * current_price - current_invested;

        if (show_trades & (j < n)) {
            Time[j] = date[i]; Cycle[j] = cycle; Price[j] = current_price;
            Price_Change[j] = 100 * ((Price[j] / price[i - 1]) - 1);
            Sold[j] = current_invested / current_avg_price;
            Bought[j] = NA_REAL; Average_Buy_Price[j] = NA_REAL;
            Dollar[j] = Sold[j] * Price[j]; Capital[j] = capital;
            Trade_Type[j] = "Stop Loss";
            j++;
        }

        time_diff = date[i] - date[base_order_buy_time];
        if (max_trade_time < time_diff) max_trade_time = time_diff;
        if (last_so_buy_time > 0) time_inactive += date[i] - date[last_so_buy_time];

        n_trades++; n_stoploss++; cycle++; reset_deal_state();

      } else if ((k == 0 && (start_asap || deal_start[i])) || (k > 0 && current_price <= buy_price)) {
        if (k < n_orders) {
          if (k == 0) {
            base_order_price = current_price;
            base_order_buy_time = i;
          }
          if (compound) capital_to_invest = capital;
          current_invested = capital_to_invest * cum_proportion[k];
          current_avg_price = base_order_price * avg_price[k];
          current_bought_coins = fee_factor * current_invested / current_avg_price;
          sell_price = base_order_price * required_change[k];
          if (stoploss_factor > 0) {
              stoploss_price = (capital * stoploss_factor - capital + current_invested) / (fee_factor * current_bought_coins);
          }
          if (show_trades & (j < n)) {
            Time[j] = date[i]; Cycle[j] = cycle; Price[j] = current_price;
            Dollar[j] = capital_to_invest * proportion[k];
            Bought[j] = fee_factor * Dollar[j] / Price[j];
            Sold[j] = NA_REAL; Average_Buy_Price[j] = current_avg_price;
            Price_Change[j] = 100 * ((Price[j] / base_order_price) - 1);
            Capital[j] = capital - current_invested +
              fee_factor * current_bought_coins * Price[j];
            Trade_Type[j] = "Buy";
            j++;
          }
          k++;
          if (k < n_orders) {
            buy_price = base_order_price * buy_price_factor[k];
            last_so_buy_time = 0;
          } else {
            last_so_buy_time = i;
            buy_price = 0;
          }
          current_bottom = current_price;
        }
      }
    } 
    else if (k > 0 && current_price >= sell_price) {
      if (trailing_enabled) {
        trailing_active = true;
        highest_price_since_activation = current_price;
      } else {
        lowest_capital = capital + fee_factor * current_bought_coins * current_bottom - current_invested;
        if (max_draw_down > (lowest_capital / highest_capital)) max_draw_down = lowest_capital / highest_capital;
        
        capital += fee_factor * current_bought_coins * sell_price - current_invested;
        if (capital > highest_capital) highest_capital = capital;
        
        if (show_trades & (j < n)) {
            Time[j] = date[i]; Cycle[j] = cycle; Price[j] = sell_price;
            Price_Change[j] = 100 * ((Price[j] / price[i-1]) - 1);
            Sold[j] = current_invested / current_avg_price;
            Bought[j] = NA_REAL; Average_Buy_Price[j] = NA_REAL;
            Dollar[j] = Sold[j] * Price[j]; Capital[j] = capital;
            Trade_Type[j] = "Take Profit";
            j++;
        }

        time_diff = date[i] - date[base_order_buy_time];
        if (max_trade_time < time_diff) max_trade_time = time_diff;
        if (last_so_buy_time > 0) time_inactive += date[i] - date[last_so_buy_time];

        n_trades++; cycle++; reset_deal_state();
      }
    }
  }

  // Add points for plotting
  if (plot) {
    for (int i = plot_step; i < i_end; i += plot_step) {
      if(j < n) {
        Price[j] = price[i];
        Time[j] = date[i];
        Cycle[j] = NA_REAL;
        Sold[j] = NA_REAL;
        Trade_Type[j] = NA_STRING;
        j += 1;
      }
    }
  }

  // --- Final Calculations ---
  final_capital = capital;
  if (k > 0) { 
    final_capital = capital + fee_factor * current_bought_coins * price[i_end - 1] - current_invested;
    lowest_capital = capital + fee_factor * current_bought_coins * current_bottom - current_invested;
  }
  
  if (final_capital > highest_capital) highest_capital = final_capital;
  if (lowest_capital < highest_capital && max_draw_down > (lowest_capital / highest_capital)) {
     max_draw_down = lowest_capital / highest_capital;
  }
  max_draw_down = 100.0 * (1.0 - max_draw_down);

  profit = 100.0 * ((final_capital / required_capital) - 1.0);
  
  if (k > 0) {
    time_diff = date[i_end - 1] - date[base_order_buy_time];
    if (max_trade_time < time_diff) max_trade_time = time_diff;
    if (k >= n_orders) {
      time_inactive += date[i_end - 1] - date[last_so_buy_time];
    }
  }

  time_inactive = (i_end > 1) ? (10000.0 * time_inactive / (date[i_end - 1] - date[0])) : 0.0;
  time_inactive = round(time_inactive) / 100.0;

  DataFrame df = DataFrame::create(Named("profit") = profit,
                                   Named("n_trades") = n_trades,
                                   Named("max_draw_down") = max_draw_down,
                                   Named("required_capital") = required_capital,
                                   Named("covered_deviation") = dev[n_orders - 1],
                                   Named("down_tolerance") = down_tolerance,
                                   Named("max_time") = max_trade_time / 86400.0,
                                   Named("percent_inactive") = time_inactive,
                                   Named("n_stoploss") = n_stoploss,
                                   Named("n_emergency_stops") = n_emergency_stops);
  List L = List::create(df, trades);
  return L;
}
