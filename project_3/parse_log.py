#!/usr/bin/env python

__author__ = "Rambod Rahmani <rambodrahmani@autistici.org>"
__license__ = "GPLv3"

# loading in the pandas module as 'pd'
import pandas as pd

# reading in the log file
git_log = pd.read_csv("git.log", sep="|", encoding='latin-1', header=None, names = ['timestamp', 'author'])

# printing out the first 5 rows
print("Data imported correctly.\n")
print(git_log.head())
print("...\n")

# calculating number of commits
number_of_commits = len(git_log)

# calculating number of authors
number_of_authors = len(git_log['author'].dropna().unique())

# printing out the results
print("%s authors committed %s code changes." % (number_of_authors, number_of_commits))

# Identifying the top 10 authors
top_10_authors = git_log['author'].value_counts().head(10)

# Listing contents of 'top_10_authors'
print("\nTOP 10 Authors:")
print(top_10_authors.head(10))
print("")

# converting the timestamp column
git_log['timestamp'] = pd.to_datetime(git_log['timestamp'], unit="s")

# summarizing the converted timestamp column
print(git_log['timestamp'].describe())
print("")

# determining the first real commit timestamp
first_commit_timestamp = git_log.iloc[-1]['timestamp']

# determining the last sensible commit timestamp
last_commit_timestamp = pd.to_datetime('2021')

# filtering out wrong timestamps
corrected_log = git_log[
    (git_log['timestamp'] >= first_commit_timestamp) &
    (git_log['timestamp'] <= last_commit_timestamp)]

# summarizing the corrected timestamp column
print(corrected_log['timestamp'].describe())
print("")

# counting the no. commits per year
commits_per_year = corrected_log.groupby(
    pd.Grouper(key='timestamp', freq='M')).count()

# listing the first rows
print(commits_per_year)

# dump to csv file
commits_per_year.to_csv("tabella.csv")
