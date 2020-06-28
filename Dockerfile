FROM mcr.microsoft.com/dotnet/core/aspnet:3.1-alpine
COPY /deploy /
WORKDIR /Server
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]