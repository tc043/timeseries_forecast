#!/bin/bash
set -e

echo "========================================="
echo "Running Time Series Project Pipeline"
echo "========================================="

# 1. Run Unit Tests
echo "Step 1: Running unit tests via testthat..."
Rscript tests/testthat.R
echo "✅ Unit tests completed successfully!"
echo "-----------------------------------------"

# 2. Run CLI Pipeline and Compile Report
echo "Step 2: Executing forecasting pipeline and compiling report..."
Rscript main.R --ratio=0.7 --freq=7 --horizon=30 --report=report.html
echo "✅ CLI pipeline executed and report generated at report.html!"
echo "-----------------------------------------"

echo "Pipeline execution finished successfully!"
echo ""
echo "To launch the interactive Shiny Dashboard, run:"
echo "  Rscript -e \"shiny::runApp('.', port = 8080)\""
echo "========================================="
