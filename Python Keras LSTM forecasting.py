import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from keras.models import Sequential
from keras.layers import LSTM, Dense
from keras.optimizers import Adam, SGD
import matplotlib.pyplot as plt
from scikeras.wrappers import KerasRegressor
from sklearn.model_selection import GridSearchCV

haul_df = pd.read_csv("/content/Haul.csv")


# Loop lists
loads = ["Passengers", "Freight", "Mail"]
business_models = ["FSC", "LCC", "Cargo", "SCC", "Air Taxi"]
hauls = ["Very Short", "Short", "Medium", "Long"]

def calculate_mape(actual, predicted):
    ape = np.abs((actual - predicted) / actual)
    mape = np.mean(ape)
    return mape

def calculate_rsquared(actual, fitted):
    r_squared = 1 - np.sum((actual - fitted)**2) / np.sum((actual - np.mean(actual))**2) if np.sum(
        (actual - np.mean(actual))**2) != 0 else 1 - np.sum((actual - fitted)**2) / 0.000001
    return r_squared

def create_train_lstm_model(train_x, train_y):
    model = Sequential()
    model.add(LSTM(units=50, input_shape=(1, train_x.shape[2])))
    model.add(Dense(units=1))

    model.compile(loss='mean_squared_error', optimizer=Adam(), metrics=['mean_absolute_percentage_error'])

    model.fit(
        x=train_x,
        y=train_y,
        epochs=50,
        batch_size=32,
        verbose=0
    )
    return model

def make_lstm_predictions(model, test_data):
     predictions = model.predict(test_data)
     predictions_flat = predictions.flatten()
     return predictions_flat

# Data frame initialization
r2_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM'])
mape_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM'])

# Loop over business models and hauls
for business_model in business_models:  # Assuming business_models is defined
    for haul in hauls:  # Assuming hauls is defined
        subset_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)]

        for load in loads:
            ts_data = subset_data[[load, 'Mean_Airlines']].to_numpy()

            # Train-test split
            train_size = int(0.8 * len(ts_data))
            train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

            # Reshape X_train and y_train
            X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1]-1))
            y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

            # Create and train LSTM model
            lstm_model = create_train_lstm_model(X_train, y_train)

            # Make predictions
            lstm_predictions = make_lstm_predictions(lstm_model, test_set)
            lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

            # Calculate metrics
            mape = calculate_mape(actual, lstm_predictions)
            r2 = calculate_rsquared(train_set[:, 0], lstm_fitted)

            # Save metrics to data frame
            mape_df = pd.concat([mape_df, pd.DataFrame([[business_model, load, haul, mape]], columns=mape_df.columns)])
            r2_df = pd.concat([r2_df, pd.DataFrame([[business_model, load, haul, r2]], columns=r2_df.columns)])

MAPE = pd.read_csv("/content/MAPE.csv")
R2 = pd.read_csv("/content/R2.csv")

#Before Grid Search
MAPE_BGS = pd.merge(MAPE, mape_df, on=['Business_Model', 'Load', 'Haul'])
R2_BGS = pd.merge(R2, r2_df, on=['Business_Model', 'Load', 'Haul'])
R2_BGS.to_csv("R2-BGS.csv", index=False)
MAPE_BGS.to_csv("MAPE-BGS.csv", index=False)

MAPE_BGS = pd.read_csv("/content/MAPE-BGS.csv")
R2_BGS = pd.read_csv("/content/R2-BGS.csv")

# Data frame initialization
r2_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM-C'])
mape_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM_C'])

# Loop over business models and hauls
for business_model in business_models:  # Assuming business_models is defined
    for haul in hauls:  # Assuming hauls is defined
        subset_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)]

        for load in loads:
            ts_data = subset_data[[load, 'Mean_Competition']].to_numpy()

            # Train-test split
            train_size = int(0.8 * len(ts_data))
            train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

            # Reshape X_train and y_train
            X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1]-1))
            y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

            # Create and train LSTM model
            lstm_model = create_train_lstm_model(X_train, y_train)

            # Make predictions
            lstm_predictions = make_lstm_predictions(lstm_model, test_set)
            lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

            # Calculate metrics
            mape = calculate_mape(actual, lstm_predictions)
            r2 = calculate_rsquared(train_set[:, 0], lstm_fitted)

            # Save metrics to data frame
            mape_df = pd.concat([mape_df, pd.DataFrame([[business_model, load, haul, mape]], columns=mape_df.columns)])
            r2_df = pd.concat([r2_df, pd.DataFrame([[business_model, load, haul, r2]], columns=r2_df.columns)])

#Competition BGS
MAPE_BGS_C = pd.merge(MAPE_BGS, mape_df, on=['Business_Model', 'Load', 'Haul'])
R2_BGS_C = pd.merge(R2_BGS, r2_df, on=['Business_Model', 'Load', 'Haul'])
R2_BGS_C.to_csv("R2-BGS-C.csv", index=False)
MAPE_BGS_C.to_csv("MAPE-BGS-C.csv", index=False)

business_model = 'LCC'
haul = 'Medium'
load = 'Passengers'
exog_variable = 'Mean_Competition'

ts_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)][[load, exog_variable]].to_numpy()

# Train-test split
train_size = int(0.8 * len(ts_data))
train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

# Reshape X_train and y_train
X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1] - 1))
y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

# Create and train LSTM model
lstm_model = create_train_lstm_model(X_train, y_train)

# Make predictions
lstm_predictions = make_lstm_predictions(lstm_model, test_set)
lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

# Plot actual vs forecasted values for one sample
plt.figure(figsize=(10, 6))
plt.plot(actual, label='Actual')
plt.plot(lstm_predictions, label='Forecasted')
plt.title(f'{business_model} - {haul} - {load}\n LSTM-{exog_variable} Forecast')
plt.legend()
plt.show()

plt.figure(figsize=(10, 6))
plt.plot(train_set[:, 0], label='Actual')
plt.plot(lstm_fitted, label='Fitted')
plt.title(f'{business_model} - {haul} - {load}\n LSTM-{exog_variable} Fit')
plt.legend()
plt.show()

 GRID **SEARCH**

# Function to create and return an LSTM model
def create_lstm_model(units=50, optimizer='adam', loss='mean_squared_error', metrics=['mean_absolute_error'], batch_size=32):
    model = Sequential()
    model.add(LSTM(units=50, input_shape=(1, X_train.shape[2])))
    model.add(Dense(units=1))

    model.compile(loss=loss, optimizer=optimizer, metrics=metrics)

    return model

# Wrap the custom wrapper function for use with scikit-learn
lstm_regressor = KerasRegressor(build_fn=create_lstm_model, epochs=50, verbose=0, units=50)

# Define the parameter grid for grid search
param_grid = {
    'units': [20, 50, 100, 200],
    'optimizer': ['adam', 'rmsprop', 'sgd'],
    'loss': ['mean_squared_error', 'mean_absolute_error', 'mean_absolute_percentage_error'],
    'batch_size': [8, 16, 32, 64],
    'epochs': [100, 250, 500]

}

# Create the GridSearchCV object
grid_search = GridSearchCV(estimator=lstm_regressor, param_grid=param_grid, scoring='neg_mean_squared_error', cv=3)

# Input data
subset_data = haul_df[(haul_df['Business_Model'] == "FSC") & (haul_df['Haul'] == "Short")]
ts_data = subset_data[["Passengers", 'Mean_Airlines']].to_numpy()
train_size = int(0.8 * len(ts_data))
train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

# Reshape X_train and y_train
X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1]-1))
y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

# Fit the grid search to the data
grid_search_result = grid_search.fit(X_train, y_train)

# Print the best parameters and corresponding score
print("Best parameters: ", grid_search_result.best_params_)
print("Best score: ", -grid_search_result.best_score_)

def create_train_lstm_model(train_x, train_y):
    model = Sequential()
    model.add(LSTM(units=100, input_shape=(1, train_x.shape[2])))
    model.add(Dense(units=1))

    model.compile(loss='mean_squared_error', optimizer=SGD(), metrics=['mean_absolute_percentage_error'])

    model.fit(
        x=train_x,
        y=train_y,
        epochs=100,
        batch_size=16,
        verbose=0
    )
    return model

def make_lstm_predictions(model, test_data):
     predictions = model.predict(test_data)
     predictions_flat = predictions.flatten()
     return predictions_flat

# Data frame initialization
r2_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM GS'])
mape_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM GS'])

# Loop over business models and hauls
for business_model in business_models:  # Assuming business_models is defined
    for haul in hauls:  # Assuming hauls is defined
        subset_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)]

        for load in loads:
            ts_data = subset_data[[load, 'Mean_Airlines']].to_numpy()

            # Train-test split
            train_size = int(0.8 * len(ts_data))
            train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

            # Reshape X_train and y_train
            X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1]-1))
            y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

            # Create and train LSTM model
            lstm_model = create_train_lstm_model(X_train, y_train)

            # Make predictions
            lstm_predictions = make_lstm_predictions(lstm_model, test_set)
            lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

            # Calculate metrics
            mape = calculate_mape(actual, lstm_predictions)
            r2 = calculate_rsquared(train_set[:, 0], lstm_fitted)

            # Save metrics to data frame
            mape_df = pd.concat([mape_df, pd.DataFrame([[business_model, load, haul, mape]], columns=mape_df.columns)])
            r2_df = pd.concat([r2_df, pd.DataFrame([[business_model, load, haul, r2]], columns=r2_df.columns)])

#After Grid Seach
MAPE_AGS = pd.merge(MAPE, mape_df, on=['Business_Model', 'Load', 'Haul'])
R2_AGS = pd.merge(R2, r2_df, on=['Business_Model', 'Load', 'Haul'])
R2_AGS.to_csv("R2-AGS.csv", index=False)
MAPE_AGS.to_csv("MAPE-AGS.csv", index=False)

# Data frame initialization
r2_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM-C GS'])
mape_df = pd.DataFrame(columns=['Business_Model', 'Load', 'Haul', 'LSTM-C GS'])

# Loop over business models and hauls
for business_model in business_models:  # Assuming business_models is defined
    for haul in hauls:  # Assuming hauls is defined
        subset_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)]

        for load in loads:
            ts_data = subset_data[[load, 'Mean_Competition']].to_numpy()

            # Train-test split
            train_size = int(0.8 * len(ts_data))
            train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

            # Reshape X_train and y_train
            X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1]-1))
            y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

            # Create and train LSTM model
            lstm_model = create_train_lstm_model(X_train, y_train)

            # Make predictions
            lstm_predictions = make_lstm_predictions(lstm_model, test_set)
            lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

            # Calculate metrics
            mape = calculate_mape(actual, lstm_predictions)
            r2 = calculate_rsquared(train_set[:, 0], lstm_fitted)

            # Save metrics to data frame
            mape_df = pd.concat([mape_df, pd.DataFrame([[business_model, load, haul, mape]], columns=mape_df.columns)])
            r2_df = pd.concat([r2_df, pd.DataFrame([[business_model, load, haul, r2]], columns=r2_df.columns)])

MAPE_AGS_C = pd.merge(MAPE_AGS, mape_df, on=['Business_Model', 'Load', 'Haul'])
R2_AGS_C = pd.merge(R2_AGS, r2_df, on=['Business_Model', 'Load', 'Haul'])
R2_AGS_C.to_csv("R2-AGS-C.csv", index=False)
MAPE_AGS_C.to_csv("MAPE-AGS-C.csv", index=False)

business_model = 'LCC'
haul = 'Medium'
load = 'Passengers'
exog_variable = 'Mean_Competition'

ts_data = haul_df[(haul_df['Business_Model'] == business_model) & (haul_df['Haul'] == haul)][[load, exog_variable]].to_numpy()

# Train-test split
train_size = int(0.8 * len(ts_data))
train_set, test_set, actual, train_fit = ts_data[:train_size, :], ts_data[train_size:, 1:], ts_data[train_size:, 0], ts_data[:train_size, 1:]

# Reshape X_train and y_train
X_train = np.reshape(train_set[:, 1:], (train_set.shape[0], 1, train_set.shape[1] - 1))
y_train = np.reshape(train_set[:, 0], (train_set.shape[0], 1))

# Create and train LSTM model
lstm_model = create_train_lstm_model(X_train, y_train)

# Make predictions
lstm_predictions = make_lstm_predictions(lstm_model, test_set)
lstm_fitted = make_lstm_predictions(lstm_model, train_fit)

# Plot actual vs forecasted values for one sample
plt.figure(figsize=(10, 6))
plt.plot(actual, label='Actual')
plt.plot(lstm_predictions, label='Forecasted')
plt.title(f'{business_model} - {haul} - {load}\n LSTM-{exog_variable} Forecast')
plt.legend()
plt.show()

plt.figure(figsize=(10, 6))
plt.plot(train_set[:, 0], label='Actual')
plt.plot(lstm_fitted, label='Fitted')
plt.title(f'{business_model} - {haul} - {load}\n LSTM-{exog_variable} Fit')
plt.legend()
plt.show()

#Test123456
#Spyder

