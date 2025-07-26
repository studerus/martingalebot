# martingalebot

[![R-CMD-check](https://github.com/studerus/martingalebot/workflows/R-CMD-check/badge.svg)](https://github.com/studerus/martingalebot/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/martingalebot)](https://CRAN.R-project.org/package=martingalebot)

## Overview

The `martingalebot` package provides comprehensive tools for downloading cryptocurrency price data from Binance and performing backtesting and parameter optimization for martingale trading strategies (also known as Dollar Cost Averaging or DCA bots). These strategies are commonly implemented by popular trading platforms such as [3commas](https://3commas.io/), [Pionex](https://www.pionex.com/), [TradeSanta](https://tradesanta.com/), [Mizar](https://mizar.com/), [OKX](https://www.okx.com/), and [Bitget](https://www.bitget.com/).

## Key Features

### 📊 Data Acquisition
- **Direct API access**: Download real-time candlestick data from Binance
- **Historical CSV data**: Fast bulk downloads from data.binance.vision
- **High-resolution data**: Support for aggregated trades with second-level precision
- **Multiple timeframes**: From 1-second to daily intervals

### 🚀 High-Performance Backtesting
- **C++ backend**: Core backtesting engine implemented in C++ for maximum speed
- **Realistic simulation**: Includes trading fees, slippage, and market conditions
- **Comprehensive metrics**: Profit, drawdown, required capital, and risk measures
- **Flexible parameters**: Customizable martingale strategy settings

### 🔧 Parameter Optimization
- **Grid search**: Systematic parameter space exploration
- **Parallel processing**: Multi-core optimization for faster results
- **Cross-validation**: Robust performance evaluation with time-series splits
- **Multiple algorithms**: Integration with genetic algorithms and simulated annealing

### 📈 Technical Analysis
- **RSI integration**: Conditional deal start based on technical indicators
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

# Download recent price data for ETHUSDT
data <- get_binance_prices_from_csv(
  symbol = "ETHUSDT",
  start_time = "2025-01-01", 
  end_time = "2025-02-01"
)

head(data)
```

### 2. Run a Simple Backtest

```r
# Backtest with default martingale parameters
results <- backtest(data = data)
print(results)

# Backtest with custom parameters
results <- backtest(
  data = data,
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
optimization_results <- grid_search(
  data = data,
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
slices <- create_timeslices(
  train_months = 4,
  test_months = 2, 
  shift_months = 1,
  data = data
)

# Perform cross-validated optimization
cv_results <- slices %>%
  group_by(start_test, end_test) %>%
  summarise({
    train_data <- filter(data, between(time, start_train, end_train))
    test_data <- filter(data, between(time, start_test, end_test))
    
    best_params <- grid_search(data = train_data)[1,]
    pmap_df(best_params, backtest, data = test_data)
  })
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

## Performance Metrics

The package provides comprehensive performance metrics:

- **Profit**: Total percentage return
- **Number of trades**: Completed deal cycles
- **Maximum drawdown**: Worst peak-to-trough loss
- **Required capital**: Total capital needed
- **Covered deviation**: Price range coverage
- **Down tolerance**: Resilience to price drops
- **Time inactive**: Percentage of time fully invested

## Advanced Features

### Technical Indicators

```r
# Add RSI-based entry conditions
data_with_rsi <- add_rsi(
  data = data,
  time_period = "1 hour",
  n = 14,
  cutoff = 30
)

# Only start deals when RSI < 30
results <- backtest(
  data = data_with_rsi,
  start_asap = FALSE
)
```

### Optimization Algorithms

```r
# Genetic Algorithm optimization
library(GA)

optimize_function <- function(params) {
  result <- backtest(
    n_safety_orders = params[1],
    pricescale = params[2],
    take_profit = params[3],
    data = data
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