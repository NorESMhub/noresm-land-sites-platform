# How to run

Clone the repo with the following command:

`git clone https://github.com/ka7eh/fates_platform_ecological_climatology --config core.autocrlf=input`

Then run the following from the project root:

`docker-compose up`

After you see a message containing `Uvicorn running on http://0.0.0.0:8000 (Press CTRL+C to quit)` in `api` docker logs, you can use the tools at `http://localhost:8000`.

You can access the Jupyter server at `http://localhost:8888/lab?token=<token>`. Replace `<token>` with the token you see in the docker logs for `jupyter` service.
