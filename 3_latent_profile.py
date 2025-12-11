import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import entropy
from scipy.linalg import det
from scipy.special import logsumexp
from scipy.stats import multivariate_normal
import math
from typing import NamedTuple
from joblib import dump
from sklearn.mixture import GaussianMixture
from sklearn.preprocessing import StandardScaler


df = pd.read_csv("./tree1721-smde-logit1.fsc", header=None ,sep="\s+")
df = df.iloc[:, [20, 22, 24, 26]]
df.columns = ["ns", "m", "d", "e"]
scaler = StandardScaler(with_mean=True, with_std=True)
scaled = scaler.fit_transform(df)

lpa_data = pd.DataFrame(scaled, columns=df.columns)
saved_path = "./results"


class ModelConfig(NamedTuple):
    """
    parameters for model setting

    """
    random_state: int = 459
    max_iter: int = 300
    covariance_type: str = "full"


def compute_gmm_entropy(gmm_model):
    """
    Approximate differential entropy of a Gaussian mixture model.

    H ≈ H(mixing weights) + Σ_k w_k * H(N(μ_k, Σ_k))

    Parameters
    ----------
    gmm_model : GaussianMixture
        Fitted sklearn GMM object.

    Returns
    -------
    float
        Approximate differential entropy of the mixture.
    """
    n_features = gmm_model.means_.shape[1] 
    mixing_entropy = entropy(gmm_model.weights_)

    component_entropies = np.sum([
        w * (np.log(det(cov)) + n_features * (1 + np.log(2 * np.pi)))
        for w, cov in zip(gmm_model.weights_, gmm_model.covariances_)
    ])

    total_entropy = mixing_entropy + 0.5 * component_entropies
    return total_entropy

def compute_component_loglikelihoods(gmm, X):
    """Return log-likelihood of each sample under each component."""
    n_components = gmm.n_components
    n_samples, n_features = X.shape
    log_probs = np.zeros((n_samples, n_components))

    for k in range(n_components):
        mean = gmm.means_[k]
        cov = gmm.covariances_[k]
        weight = gmm.weights_[k]
        log_probs[:, k] = np.log(weight) + multivariate_normal.logpdf(X, mean=mean, cov=cov)

    return log_probs

def compute_total_loglikelihood(gmm, X):
    log_probs = compute_component_loglikelihoods(gmm, X)
    return logsumexp(log_probs, axis=1)  

def compute_component_contributions(gmm, X):
    log_probs = compute_component_loglikelihoods(gmm, X)
    responsibilities = gmm.predict_proba(X)
    total_ll = logsumexp(log_probs, axis=1).sum()
    comp_ll = (responsibilities * log_probs).sum(axis=0)
    return comp_ll, total_ll

def finetune_model(data, model_config):
    aic_values = []
    bic_values = []
    n_components = []
    entropy = []
    loglikely_hoods = []

    n_profiles = range(1, 10)
    for n in n_profiles:
        print(f"------ {n} latent ------")
        model = GaussianMixture(n_components=n, covariance_type=model_config.covariance_type, random_state=model_config.random_state, max_iter=model_config.max_iter)
        model.fit(data)
        params = model.get_params()
        pred_x = model.predict_proba(data)
        _, total_ll = compute_component_contributions(model, data)
        model_entropy = compute_gmm_entropy(model)
        print("model_entropy: ", model_entropy)
        print("log-likelihood:", total_ll)

        aic_values.append(model.aic(data))
        bic_values.append(model.bic(data))
        entropy.append(model_entropy)
        loglikely_hoods.append(total_ll)
        n_components.append(n)

    return {
        "aic":aic_values,
        "bic": bic_values,
        "n_components": n_components,
        "log-likelihood": loglikely_hoods,
        "entropy":entropy,
    }


def save_finetune_result(results, path):
    df = pd.DataFrame(results)
    path = os.path.join(path, "infoCriterion.csv")
    df.to_csv(path, index=False)
    print("Saved results to infoCriterion.csv")
    return

def save_predict_labels(lpa_data, pred_labels, save_path):
    lpa_data["cluster"] = pred_labels
    lpa_data.to_csv(save_path, index=False)
    return


def build_model_and_fit(n_components, model_config):
    final_model = GaussianMixture(n_components=n_components, covariance_type=model_config.covariance_type, random_state=model_config.random_state, max_iter=model_config.max_iter)
    final_model.fit(lpa_data)
    labels = final_model.predict(lpa_data)
    return np.array(labels).astype(np.int32), final_model


def _name_saved_files(saved_path, model_config):
    
    prefix = "model_" + "cov_" + str(model_config.covariance_type)     
    save_model_folder = os.path.join(saved_path, prefix)
    if not os.path.exists(save_model_folder):
        os.mkdir(save_model_folder)
    saved_re_path = os.path.join(save_model_folder, prefix +".npy")      
    saved_model_path = os.path.join(save_model_folder, prefix +".joblib")  

    saved_path = {
        "result": saved_re_path,
        "model" : saved_model_path,
    }

    return saved_path


def main():
    model_config = ModelConfig()
    params_seraching = finetune_model(lpa_data, model_config)
    save_finetune_result(params_seraching, saved_path)
    pred_labels, model = build_model_and_fit(5, model_config)
    save_path = _name_saved_files(saved_path, model_config)

    dump(model, save_path['model'])
    save_predict_labels(lpa_data, pred_labels, saved_path +"/lpa_k5.csv")
    print("results saved")


if __name__ == "__main__":
    main()
