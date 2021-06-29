#!/usr/bin/python3

# no data for 'american_samoa',
states = ('alabama',
          'alaska',
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

wa_counties = ('adams',
               'asotin',
               'benton',
               'chelan',
               'clallam',
               'clark',
               'columbia',
               'cowlitz',
               'douglas',
               'ferry',
               'franklin',
               'garfield',
               'grant',
               'grays_harbor',
               'island',
               'jefferson',
               'king',
               'kitsap',
               'kittitas',
               'klickitat',
               'lewis',
               'lincoln',
               'mason',
               'okanogan',
               'pacific',
               'pend_oreille',
               'pierce',
               'san_juan',
               'skagit',
               'skamania',
               'snohomish',
               'spokane',
               'stevens',
               'thurston',
               'wahkiakum',
               'walla_walla',
               'whatcom',
               'whitman',
               'yakima')

def mktable(fh, thelist, prefix, reference, reference_p):
    fh.write("<style>")
    fh.write("  table,")
    fh.write("  th,")
    fh.write("  td {")
    fh.write("     padding: 10px;")
    fh.write("     border: 1px solid black;")
    fh.write("     border-collapse: collapse;")
    fh.write("  }")
    fh.write("</style>\n")
    fh.write("<TABLE>\n")
    url_base = "https://s3-us-west-2.amazonaws.com/rbucket-matsmats/"
    for s in thelist:
        t = s.title()
        t = t.replace("_Of_", " of ")
        t = t.replace("_", " ")
        fh.write("  <TR>")
    
        fh.write("    <TD>")
        fh.write("      %s" % t)
        fh.write("    </TD>")
        fh.write("    <TD>")
        fh.write("      <A HREF=\"%s%s%s_cases_per_hundy.jpg\">Cases</A>" %
              (url_base, prefix, s))
        fh.write("    </TD>")
        fh.write("    <TD>")
        fh.write("      <A HREF=\"%s%s%s_daily_cases.jpg\">Daily Cases</A>" %
              (url_base, prefix, s))
        fh.write("    </TD>")
        fh.write("    <TD>")
        fh.write("      <A HREF=\"%s%s_and_%s.jpg\">Compared to %s</A>" %
              (url_base, reference, s, reference_p))
        fh.write("    </TD>")
        fh.write("    <TD>")
        fh.write("      <A HREF=\"%s%s%s.csv\">CSV</A>" %
              (url_base, prefix, s))
        fh.write("    </TD>")

        fh.write("  </TR>\n")

    fh.write("<TABLE>")

fh = open('state_table.html', 'w')
mktable(fh, states, "", "wa", "WA")
fh = open('wacounty_table.html', 'w')
mktable(fh, wa_counties, "washington_", "king", "King")

