FROM ipds/bev-base

# copy the app to the image
RUN mkdir /root/burghs-eye-view-places
COPY burghs-eye-view-places /root/burghs-eye-view-places

CMD ["R", "-e shiny::runApp('/root/burghs-eye-view-places')"]