package net.prover.views

import net.prover.viewmodel.Breadcrumb

import scala.xml.Elem

object MainTemplate {
  def apply(breadcrumbs: Breadcrumb*)(content: Elem): Elem = {
    <html>
      <head>
        <title>Prover</title>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous" />
        <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.8.0/css/all.css" integrity="sha384-Mmxa0mLqhmOeaE8vgOSbKacftZcsNYDjQzuCOm6D02luYSzBG8vpaOykv9lFQ51Y" crossorigin="anonymous" />
        <link rel="stylesheet" href="/css/prover.css"/>
        <script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
        <script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
        <script src="https://cdnjs.cloudflare.com/ajax/libs/lodash.js/4.17.11/lodash.js"></script>
        <script src="https://twitter.github.io/typeahead.js/releases/latest/typeahead.bundle.js"></script>
      </head>
      <body>
        <div class="navbarWrapper">
          <div class="container">
            <header class="navbar navbar-expand navbar-dark bg-dark">
              <div class="navbar-header">
                <a href="/books" class="navbar-brand">Prover</a>
              </div>
            </header>
          </div>
        </div>
        { if(breadcrumbs.nonEmpty)
          <div class="breadcrumbWrapper">
            <div class="container">
              <ol class="breadcrumb">
                { breadcrumbs.map { breadcrumb =>
                  <li class={"breadcrumb-item" + (if (breadcrumb == breadcrumbs.last) " active" else "")}>
                    { if(breadcrumb != breadcrumbs.last)
                      <a href={breadcrumb.link}>{breadcrumb.text}</a>
                    else
                      <span>{breadcrumb.text}</span>
                    }
                  </li>
                }}
              </ol>
            </div>
          </div>
        }
        <div class="main container">
          {content}
        </div>
      </body>
    </html>
  }
}
