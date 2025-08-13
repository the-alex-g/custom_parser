mkdir temp
mv monsters/*.yaml temp
rm monsters/*.yam*
python3 reformater.py
rm -rf temp
