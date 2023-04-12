FROM nginx
RUN rm /etc/nginx/conf.d/default.conf
RUN rm -rf /usr/share/nginx/html

COPY nginx.conf /etc/nginx/conf.d/nginx.conf

RUN mkdir /usr/share/nginx/static
COPY ./static /usr/share/nginx/static

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
