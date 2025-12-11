#!/bin/bash
# Create a Python virtual environment and install dependencies

echo "Creating virtual environment..."
python3.12 -m venv .venv
source .venv/bin/activate
echo "Installing dependencies..."
pip install --upgrade pip
pip install pandas scikit-learn numpy matplotlib seaborn scipy joblib
echo "✅ Environment setup complete!"






