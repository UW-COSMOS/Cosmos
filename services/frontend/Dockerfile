FROM node:latest

WORKDIR /app

# add `/app/node_modules/.bin` to $PATH
ENV PATH /app/node_modules/.bin:$PATH

# install and cache app dependencies
COPY ./cosmos/package.json /app/package.json
COPY ./cosmos/yarn.lock /app/yarn.lock
RUN yarn install 
RUN yarn add react-scripts@3.0.1

RUN yarn add @material-ui/core
RUN yarn add @material-ui/icons
RUN yarn add react-vis
RUN yarn add react-syntax-highlighter

