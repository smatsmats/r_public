#!/usr/bin/python3
#https://s3-us-west-2.amazonaws.com/rbucket-matsmats/USA_cases_per_hundy.jpg
#alabama_cases_per_hundy.jpg
#wa_and_alabama.jpg
#
#for 

l = ('alabama',
'alaska',
'american_samoa',
'arizona',
'arkansas',
'california',
'colorado',
'connecticut',
'delaware',
'district_of_columbia',
'florida',
'georgia',
'guam',
'hawaii',
'idaho',
'illinois',
'indiana',
'iowa',
'kansas',
'kentucky',
'louisiana',
'maine',
'maryland',
'massachusetts',
'michigan',
'minnesota',
'mississippi',
'missouri',
'montana',
'nebraska',
'nevada',
'new_hampshire',
'new_jersey',
'new_mexico',
'new_york',
'north_carolina',
'north_dakota',
'northern_mariana_islands',
'ohio',
'oklahoma',
'oregon',
'pennsylvania',
'puerto_rico',
'rhode_island',
'south_carolina',
'south_dakota',
'tennessee',
'texas',
'utah',
'vermont',
'virgin_islands',
'virginia',
'washington',
'west_virginia',
'wisconsin',
'wyoming')

print("<style>")
print("  table,")
print("  th,")
print("  td {")
print("     padding: 10px;")
print("     border: 1px solid black;")
print("     border-collapse: collapse;")
print("  }")
print("</style>")
print("<TABLE>")
for s in l:
    t = s.title()
    t = t.replace("_Of_", " of ")
    t = t.replace("_", " ")
    print("  <TR>")
  
    print("    <TD>")
    print("      ", t)
    print("    </TD>")
    print("    <TD>")
    print("      <A HREF=\"https://s3-us-west-2.amazonaws.com/rbucket-matsmats/%s_cases_per_hundy.jpg\">Cases</A>" % (s))
    print("    </TD>")
    print("    <TD>")
    print("      <A HREF=\"https://s3-us-west-2.amazonaws.com/rbucket-matsmats/%s_daily_cases.jpg\">Daily Cases</A>" % (s))
    print("    </TD>")
    print("    <TD>")
    print("      <A HREF=\"https://s3-us-west-2.amazonaws.com/rbucket-matsmats/wa_and_%s.jpg\">Compared to WA</A>" % (s))
    print("    </TD>")

    print("  </TR>")

print("<TABLE>")