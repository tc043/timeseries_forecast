@echo off
echo =========================================
echo Running Time Series Project Pipeline (Windows)
echo =========================================

echo Step 1: Running unit tests via testthat...
Rscript tests/testthat.R
if %errorlevel% neq 0 (
    echo ❌ Unit tests failed!
    exit /b %errorlevel%
)
echo ✅ Unit tests completed successfully!
echo -----------------------------------------

echo Step 2: Executing forecasting pipeline and compiling report...
Rscript main.R --ratio=0.7 --freq=7 --horizon=30 --report=report.html
if %errorlevel% neq 0 (
    echo ❌ Pipeline execution failed!
    exit /b %errorlevel%
)
echo ✅ CLI pipeline executed and report generated at report.html!
echo -----------------------------------------

echo Pipeline execution finished successfully!
echo.
echo To launch the interactive Shiny Dashboard, run:
echo   Rscript -e "shiny::runApp('.', port = 8080)"
echo =========================================
