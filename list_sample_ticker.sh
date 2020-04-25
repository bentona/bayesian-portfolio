cat sample_6.csv | cut -d',' -f 9 | tail -n+2 | uniq | tr '\n' ' '
