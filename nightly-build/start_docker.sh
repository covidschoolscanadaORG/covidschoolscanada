#!/bin/bash

docker run -it \
	-v /home/shraddhapai/Canada_COVID_tracker:/home/shraddhapai/Canada_COVID_tracker \
	-v /home/shraddhapai/software/covidschoolscanada:/home/shraddhapai/software/covidschoolscanada \
	bioc312_netdx /bin/bash
