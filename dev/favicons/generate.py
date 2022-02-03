from favicons import Favicons

YOUR_ICON = "favicon.png"
WEB_SERVER_ROOT = "../../www"

with Favicons(YOUR_ICON, WEB_SERVER_ROOT) as favicons:
    favicons.generate()
    for icon in favicons.filenames():
        print(icon)
    with open('tags.html', 'w+') as f:
        for tag in favicons.html_gen():
            f.write('{0}\n'.format(tag))
