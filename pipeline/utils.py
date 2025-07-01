import logging
import os
from datetime import datetime


def setup_logging(log_folder = "logs"):
    # Base directory for logs
    base_log_dir = log_folder

    # Create daily folder inside logs directory
    date_str = datetime.now().strftime('%Y-%m-%d')
    daily_log_dir = os.path.join(base_log_dir, date_str)
    os.makedirs(daily_log_dir, exist_ok=True)

    # Define log file path with timestamp
    timestamp = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
    log_filename = f"experiments_{timestamp}.log"
    log_filepath = os.path.join(daily_log_dir, log_filename)

    # Get or create the logger
    logger = logging.getLogger("TogetherAI_Logger")
    logger.setLevel(logging.DEBUG)

    # Avoid adding duplicate handlers if logger is reused
    if not logger.handlers:
        # File handler
        file_handler = logging.FileHandler(log_filepath)
        file_handler.setLevel(logging.INFO)

        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)

        # Common log format
        formatter = logging.Formatter(
            '< %(asctime)s | %(levelname)s > %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)

        # Add handlers
        logger.addHandler(file_handler)
        logger.addHandler(console_handler)

    return logger
