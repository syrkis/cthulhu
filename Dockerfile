FROM mcr.microsoft.com/dotnet/sdk:6.0

ARG FP2022_PASSWORD

RUN dotnet nuget \
    add source https://nuget.pkg.github.com/jesper-bengtson/index.json \
    -n FP2022\
    -u jesper-bengtson\
    -p ${FP2022_PASSWORD}\
    --store-password-in-clear-text
