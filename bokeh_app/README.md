# Instructions
## 1 Run a bokeh-app via ssh-tunnel
#### 1.1 On the remote machine
Run the following:
```
python3 -m bokeh serve --show [name_of_app] & ssh -NfR 5006:localhost:5006 [user@remote.ip]
```
where [name_of_app] is the name of the folder containing the app you want to run and [user@remote.ip] is the address of the remote machine. For example:

```
python3 -m bokeh serve --show make_settings & ssh -NfR 5006:localhost:5006 centos@158.39.201.200
```
See `./start_app_server.sh` for an example.

#### 1.2 On the local machine
Run the following:
```
ssh -NfL localhost:5006:localhost:5006 [user@remote.ip]
```
where [user@remote.ip] must be identical to what was used in 1.1. For example:
```
ssh -NfL localhost:5006:localhost:5006 centos@158.39.201.200
```

To kill the process on both the remote and a local machine, use:
```
fuser -k 5006/tcp
```
assuming the port `5006` specified in 1.1 and 1.2 was not adapted.
