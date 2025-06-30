---
date: '{{ .Date }}'
draft: true
title: '{{ replace .Name "-" " " | title }}'
tags:
- programming
publishDate: '{{ time.Now.AddDate 0 1 0 | time.Format "2006-01-02" }}'
summary: ""
---