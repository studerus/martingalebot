# martingalebot

[![R-CMD-check](https://github.com/studerus/martingalebot/workflows/R-CMD-check/badge.svg)](https://github.com/studerus/martingalebot/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/martingalebot)](https://CRAN.R-project.org/package=martingalebot)

## Overview

The `martingalebot` package provides comprehensive tools for downloading cryptocurrency price data from Binance and performing backtesting and parameter optimization for martingale trading strategies (also known as Dollar Cost Averaging or DCA bots). These strategies are commonly implemented by popular trading platforms such as [3commas](https://3commas.io/), [Pionex](https://www.pionex.com/), [TradeSanta](https://tradesanta.com/), [Mizar](https://mizar.com/), [Binance](https://www.binance.com/), [OKX](https://www.okx.com/), [Crypto.com](https://crypto.com/), and [Bitget](https://www.bitget.com/).

## Key Features

### 📊 Data Acquisition
- **Direct API access**: Download real-time candlestick data from Binance
- **Historical CSV data**: Fast bulk downloads from data.binance.vision
- **High-resolution data**: Support for aggregated trades with second-level precision
- **Multiple timeframes**: From 1-second to daily intervals

### 🚀 High-Performance Backtesting
- **C++ backend**: Core backtesting engine implemented in C++ for maximum speed
- **Realistic simulation**: Includes trading fees, slippage, and market conditions
- **Advanced risk management**: Support for Stop Loss, Trailing Take Profit, and external Emergency Stop signals.
- **Comprehensive metrics**: Profit, drawdown, required capital, and risk measures
- **Flexible parameters**: Customizable martingale strategy settings

### 🔧 Parameter Optimization
- **Grid search**: Systematic parameter space exploration
- **Parallel processing**: Multi-core optimization for faster results
- **Cross-validation**: Robust performance evaluation with time-series splits
- **Multiple algorithms**: Integration with genetic algorithms and simulated annealing

### 📈 Technical Analysis
- **Deal Start Filters**: Use SMA, RSI, Bollinger Bands, and MACD as trend or entry filters.
- **Custom conditions**: Flexible entry/exit signal implementation
- **Visualization**: Interactive plots with trade execution details

## Installation

You can install the development version of martingalebot from GitHub:

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("studerus/martingalebot")
```

## Quick Start

### 1. Download Price Data

```r
library(martingalebot)
library(dplyr) # For the pipe operator and other utilities

# Download recent price data for ETHUSDT
dat <- get_binance_prices_from_csv(
  symbol = "ETHUSDT",
  start_time = "2025-01-01", 
  end_time = "2025-02-01"
)

head(dat)
```

### 2. Run a Simple Backtest

```r
# Backtest with default martingale parameters using the pipe
results <- dat |> backtest()
print(results)

# Backtest with custom parameters and plot the results
dat |> 
  backtest(
    base_order_volume = 50,
    first_safety_order_volume = 50,
    n_safety_orders = 8,
    pricescale = 2.5,
    take_profit = 2.0,
    plot = TRUE
  )
```

### 3. Optimize Parameters

```r
# Grid search for optimal parameters
optimization_results <- dat |>
  grid_search(
    n_safety_orders = 6:10,
    pricescale = c(2.0, 2.5, 3.0),
    take_profit = c(1.5, 2.0, 2.5)
  )

# View best performing parameters
head(optimization_results)
```

### 4. Cross-Validation

```r
# Create time slices for cross-validation
slices <- dat |>
  create_timeslices(
    train_months = 4,
    test_months = 2, 
    shift_months = 1
  )

# Perform cross-validated optimization
cv_results <- slices |>
  group_by(start_test, end_test) |>
  summarise({
    train_data <- filter(dat, between(time, start_train, end_train))
    test_data <- filter(dat, between(time, start_test, end_test))
    
    best_params <- train_data |>
      grid_search() |>
      slice(1)
      
    exec(backtest, !!!best_params, data = test_data)
  }, .groups = "drop")
```

## Strategy Parameters

The martingale strategy can be customized with the following parameters:

| Parameter | Description | Default |
|-----------|-------------|---------|
| `base_order_volume` | Initial order size (quote currency) | 10 |
| `first_safety_order_volume` | First safety order size | 10 |
| `n_safety_orders` | Maximum number of safety orders | 8 |
| `pricescale` | Price deviation for safety orders (%) | 2.4 |
| `volumescale` | Volume multiplier for safety orders | 1.5 |
| `take_profit` | Profit target (%) | 2.4 |
| `stepscale` | Price deviation multiplier | 1 |
| `stoploss` | Stop loss threshold (%) | 0 (disabled) |
| `trailing_take_profit` | Enable trailing take profit | `FALSE` |
| `trailing_rate` | Price drop from peak to trigger trailing sell (%) | 0.2 |
| `use_emergency_stop` | Enable emergency stop signal | `FALSE` |


## Performance Metrics

The package provides comprehensive performance metrics:

- **Profit**: Total percentage return
- **Number of trades**: Completed deal cycles
- **Maximum drawdown**: Worst peak-to-trough loss
- **Required capital**: Total capital needed
- **Covered deviation**: Price range coverage
- **Down tolerance**: Resilience to price drops
- **Time inactive**: Percentage of time fully invested
- **n_stoploss**: The number of stoplosses that had been triggered
- **n_emergency_stops**: The number of emergency stops that had been triggered

## Advanced Features

### Risk Management Features

#### Trailing Take Profit
A trailing take profit can be used to maximize gains in an uptrend. When `trailing_take_profit = TRUE`, instead of closing the deal at a fixed `take_profit` percentage, the bot will wait for the price to drop by `trailing_rate` from its highest point since the take profit level was first reached.

```r
# Use a trailing take profit that sells after a 0.5% drop from the peak
dat |>
  backtest(
    trailing_take_profit = TRUE,
    trailing_rate = 0.005 # 0.5%
  )
```

#### Emergency Stop
The `emergency_stop` feature allows for immediate closure of any open deal based on an external signal. This is useful for reacting to major market events (e.g., high volatility, black swan events). To use it, you must add a logical column named `emergency_stop` to your data and set `use_emergency_stop = TRUE`.

```r
# Add a custom emergency signal (e.g., based on extreme RSI values)
data_with_signal <- dat |>
  add_rsi_filter(
    n = 14, 
    time_period = "1 hour", 
    cutoff = 80, 
    rsi_is_above = TRUE, # Signal when RSI is ABOVE 80
    column_name = "emergency_stop"
  )

# Run backtest using the emergency stop signal
data_with_signal |>
  backtest(use_emergency_stop = TRUE)
```


### Technical Indicators

The package includes several `add_*_filter` functions (`add_sma_filter`, `add_rsi_filter`, `add_bollinger_filter`, `add_macd_filter`) to generate deal start signals based on common technical indicators.

```r
# Add RSI-based entry conditions and backtest in a single pipeline
dat |>
  add_rsi_filter(time_period = "1 hour", n = 14, cutoff = 30) |>
  backtest(start_asap = FALSE)
```

### Optimization Algorithms

```r
# Genetic Algorithm optimization
library(GA)

optimize_function <- function(params) {
  # The GA passes an unnamed vector, so we assign names for backtest
  names(params) <- c("n_safety_orders", "pricescale", "take_profit")
  
  result <- dat |>
    backtest(
      n_safety_orders = params["n_safety_orders"],
      pricescale = params["pricescale"],
      take_profit = params["take_profit"]
    )
  return(result$profit)
}

ga_result <- ga(
  type = "real-valued",
  fitness = optimize_function,
  lower = c(6, 1.5, 1.0),
  upper = c(12, 3.5, 3.0),
  maxiter = 100
)
```

## Documentation

- **📖 Online Vignette**: [Comprehensive tutorial with examples](https://studerus.github.io/martingalebot/) (recommended for best viewing experience)
- **Vignette**: R package vignette (`vignette("Introduction", package = "martingalebot")`)
- **Function documentation**: Help pages for all functions (`?backtest`, `?grid_search`, etc.)
- **Examples**: Practical use cases in function documentation

## Dependencies

The package builds on several high-quality R packages:

- **Core**: `dplyr`, `data.table`, `Rcpp`
- **Parallel processing**: `future`, `furrr`
- **Visualization**: `ggplot2`, `plotly`
- **Financial data**: `binancer`, `TTR`, `tidyquant`

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the GPL (>= 2) License - see the LICENSE file for details.

## Disclaimer

This software is for educational and research purposes only. Cryptocurrency trading involves substantial risk of loss. Past performance does not guarantee future results. The authors are not responsible for any financial losses incurred through the use of this software.

## Citation

If you use this package in your research, please cite:

```
@Manual{,
  title = {martingalebot: Martingale Trading Strategy Backtesting},
  author = {[Author Name]},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/studerus/martingalebot},
}
```

## Support

- **Issues**: Report bugs or request features on [GitHub Issues](https://github.com/studerus/martingalebot/issues)
- **Discussions**: Ask questions in [GitHub Discussions](https://github.com/studerus/martingalebot/discussions)
- **Email**: Contact the maintainer at erich.studerus@gmail.com
