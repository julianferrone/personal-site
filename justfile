# Deploy the website
deploy:
    git push

# Run website locally (including content marked as draft)
run:
    hugo server --watch --buildDrafts
