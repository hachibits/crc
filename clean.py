import math

import numpy as np
import pandas as pd


def clean_filbin_patient_data():
    df = pd.read_excel("data/original/Filbin/Clinical Metadata.xlsx")

    # Create id field with F prefix
    df["id"] = df["Public ID"].map("F{0:03d}".format)

    # Categorise data
    df["age"] = df["Age cat"].map(
        {1: "20-34", 2: "35-49", 3: "50-64", 4: "65-79", 5: "80+"}
    )
    df["bmi"] = df["BMI cat"].map(
        {
            0: "underweight",
            1: "normal",
            2: "overweight",
            3: "obese",
            4: "severely obese",
            5: np.nan,
        }
    )

    # average multi day readings
    averaged_data = (
        (
            "white_blood_cell",
            "abs_neut_",
            {1: "0-0.99", 2: "1-3.99", 3: "4-7.99", 4: "8-11.99", 5: "12+"},
        ),
        (
            "lymphocyte",
            "abs_lymph_",
            {1: "0-0.49", 2: "0.5-0.99", 3: "1-1.49", 4: "1.5-1.99", 5: "2+"},
        ),
        (
            "monocyte",
            "abs_mono_",
            {1: "0-0.24", 2: "0.25-0.49", 3: "0.5-0.74", 4: "0.75-0.99", 5: "1+"},
        ),
        (
            "creatine",
            "creat_",
            {1: "0-0.79", 2: "0.8-1.19", 3: "1.2-1.79", 4: "1.8-2.99", 5: "3+"},
        ),
        (
            "c_reactive_protein",
            "crp_",
            {1: "0-19", 2: "20-59", 3: "60-99", 4: "100-179", 5: "180+"},
        ),
    )
    for new_name, prefix, categories in averaged_data:
        df[new_name] = (
            df[[prefix + str(i) + "_cat" for i in [0, 3, 7]]]
            .mean(axis=1)
            .round()
            .map(categories)
        )

    # Determine COVID severity
    df["group"] = np.where(df["COVID"] == 0, "non-COVID-19", "non-Severe")
    df.loc[(df["Acuity max"] <= 2) & (df["group"] == "non-Severe"), "group"] = "Severe"

    # Select only relevant columns
    df = df[
        [
            "id",
            "group",
            "age",
            "bmi",
            "white_blood_cell",
            "lymphocyte",
            "monocyte",
            "c_reactive_protein",
            "creatine",
        ]
    ]
    return df


def clean_shen_patient_data():
    """Renames variables to be R appropriate, provides levels to factors, cleans N/As"""

    # Read data with only selected columns
    df = pd.read_excel("data/original/Shen/mmc1.xlsx", sheet_name=1)
    df = df.rename(
        columns={
            "Patient ID a": "id",
            "Group d": "group",
            "Age (year)": "age",
            "BMI h": "bmi",
            "WBC count, ×109/L": "white_blood_cell",
            "Lymphocyte count, ×109/L": "lymphocyte",
            "Monocyte count, ×109/L": "monocyte",
            "CRP i, mg/L": "c_reactive_protein",
            "Creatinine, μmol/L": "creatine",
        }
    )
    df = df[
        [
            "id",
            "group",
            "age",
            "bmi",
            "white_blood_cell",
            "lymphocyte",
            "monocyte",
            "c_reactive_protein",
            "creatine",
        ]
    ]

    # Clean
    df = df.replace("/", np.nan)
    df["group"] = df["group"].replace(
        {0: "healthy", 1: "non-COVID-19", 2: "non-Severe", 3: "Severe"}
    )
    df["id"] = "S" + df["id"].str.replace("-", "_")
    df["c_reactive_protein"] = pd.to_numeric(df["c_reactive_protein"], errors="coerce")

    # Convert numeric data to categorical
    categorical_data = (
        (
            "age",
            [20, 35, 50, 65, 80, math.inf],
            ["20-34", "35-49", "50-64", "65-79", "80+"],
        ),
        (
            "bmi",
            [0, 18.5, 25, 30, 40, math.inf],
            ["underweight", "normal", "overweight", "obese", "severely obese"],
        ),
        (
            "white_blood_cell",
            [0, 1, 4, 8, 12, math.inf],
            ["0-0.99", "1-3.99", "4-7.99", "8-11.99", "12+"],
        ),
        (
            "lymphocyte",
            [0, 0.5, 1, 1.5, 2, math.inf],
            ["0-0.49", "0.5-0.99", "1-1.49", "1.5-1.99", "2+"],
        ),
        (
            "monocyte",
            [0, 0.25, 0.5, 0.75, 1, math.inf],
            ["0-0.24", "0.25-0.49", "0.5-0.74", "0.75-0.99", "1+"],
        ),
        (
            "c_reactive_protein",
            [0, 20, 60, 100, 180, math.inf],
            ["0-19", "20-59", "60-99", "100-179", "180+"],
        ),
        (
            "creatine",
            [20, 35, 50, 65, 80, math.inf],
            ["20-34", "35-49", "50-64", "65-79", "80+"],
        ),
    )
    for col, bins, labels in categorical_data:
        df[col] = pd.cut(df[col], bins, labels=labels, include_lowest=True)

    return df


def clean_shen_omics_data():
    """Merges MMC3, MMC4 and MMC5 into a singular dataframe"""
    df = None
    for file in [
        "data/original/Shen/mmc3.xlsx",
        "data/original/Shen/mmc4.xlsx",
        "data/original/Shen/mmc5.xlsx",
    ]:
        new = pd.read_excel(file, sheet_name=1, header=1)
        new = new.rename(columns={new.columns[0]: "proteins"}).drop(
            new.columns[1], axis=1
        )
        if df is None:
            df = new
        else:
            df = df.merge(new, on="proteins", how="outer")

    cols = list(df.columns)
    for i in range(1, len(cols)):
        cols[i] = "S" + cols[i]
    df.columns = cols
    df = (
        df.set_index("proteins")
        .transpose()
        .reset_index()
        .rename(columns={"index": "id"})
    )
    return df


def clean_filbin_omics_data():
    df = (
        pd.read_excel("data/original/Filbin/Somalogic Proteomics.xlsx")
        .drop("sample_barcode", axis=1)
        .groupby("Public")
        .mean()
        .reset_index()
        .rename(columns={"Public": "id"})
    )
    df["id"] = df["id"].map("F{0:03d}".format)
    return df


shen_data = pd.merge(
    clean_shen_patient_data(), clean_shen_omics_data().drop("creatine", axis=1), on="id"
)
filbin_data = pd.merge(clean_filbin_patient_data(), clean_filbin_omics_data(), on="id")

matching_columns = sorted(
    list(set(list(shen_data.columns)).intersection(list(filbin_data.columns))),
    reverse=True,
)

shen_data = shen_data[matching_columns]
filbin_data = filbin_data[matching_columns]

shen_data.to_csv("data/cleaned/shen_data.csv", index=False)
filbin_data.to_csv("data/cleaned/filbin_data.csv", index=False)
