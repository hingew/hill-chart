#########################################
### Builder stage
#########################################
FROM node:lts-alpine as builder

RUN mkdir /app
WORKDIR /app
COPY . .

RUN npm ci
RUN npm run build

#########################################
### Release Stage
#########################################
FROM nginx:alpine as release

COPY --from=builder /app/index.html /usr/share/nginx/html/
COPY ./docker/robots.txt /usr/share/nginx/html/robots.txt
COPY ./docker/nginx.conf /etc/nginx/conf.d/default.conf


