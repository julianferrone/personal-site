---
date: "2025-05-05T19:44:26+10:00"
draft: true
title: "Matryoshka 3: Persisting Data to Disk"
tags:
- elixir
- programming
publishDate: 2025-06-01
---

- FilesystemStore
-- issues: only works with stringified keys / values, extremely naive, uses one file for every key
- LogStore
-- inspired by https://www.poeticoding.com/build-a-simple-persistent-key-value-store-in-elixir-using-logs-part-1/
-- 
- PersistentStore as example of pre-composed store