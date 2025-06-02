#!/usr/bin/env bash

# Clear old files
echo "Clear pdfs from previous runs..."
rm results/*.pdf
echo "Clear rda files from previous runs..."
rm results/*.rda
echo "Clear log files from previous runs..."
rm results/pipeline_log.txt

# Log file name with timestamp
LOGFILE="results/pipeline_log.txt"

# scripts to run
SCRIPTS=(
  "00_install_pkgs.R"
  "01_plot_descrip.R"
  "02_run_model.R"
  "03_plot_coefs.R"
  "04_plot_influence.R"
  "05_run_perf.R"
  "06_plot_perf.R"
  "07_plot_convergence.R"
)

# Write header to log file and terminal
echo "🚀 Pipeline started at $(date)"
echo "📄 Logging to $LOGFILE"
{
  echo "🚀 Pipeline started at $(date)"
  echo "----------------------------------------"
} >> "$LOGFILE"

# iter 
for script in "${SCRIPTS[@]}"
do
  start_time=$(date)
  echo "🔹 Starting $script at $start_time"
  echo "🔹 Starting $script at $start_time" >> "$LOGFILE"

  {
    echo "========== BEGIN: $script =========="
    echo "[$start_time] Starting $script"
    Rscript "$script"
    echo "========== END: $script =========="
    echo "[$(date)] Finished $script"
    echo "----------------------------------------"
  } >> "$LOGFILE" 2>&1

  echo "✅ Finished $script at $(date)"
done

# Final message
end_time=$(date)
echo "🏁 Pipeline completed at $end_time"
echo "🏁 Pipeline completed at $end_time" >> "$LOGFILE"
echo "📄 Full log saved to $LOGFILE"
