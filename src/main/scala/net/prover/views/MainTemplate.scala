package net.prover.views

import net.prover.viewmodel.Breadcrumb

import scala.xml.Elem

object MainTemplate {
  def apply(breadcrumbs: Breadcrumb*)(content: Elem): Elem = {
    <html>
      <head>
        <title>Prover</title>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <link rel="stylesheet" href="/webjars/bootstrap/3.3.7/css/bootstrap.css"/>
        <link rel="stylesheet" href="/css/prover.css"/>
        <script src="/webjars/jquery/3.1.1/jquery.js"></script>
        <script src="/webjars/lodash/4.15.0/lodash.js"></script>
        <script src="/webjars/bootstrap/3.3.7/js/bootstrap.js"></script>
        <script src="https://twitter.github.io/typeahead.js/releases/latest/typeahead.bundle.js"></script>
      </head>
      <body>
        <header class="navbar navbar-static-top navbar-inverse">
          <div class="container">
            <div class="navbar-header">
              <a href="/books" class="navbar-brand">Prover</a>
            </div>
          </div>
        </header>
        { if(breadcrumbs.nonEmpty)
          <div class="breadcrumbWrapper">
            <div class="container">
              <ol class="breadcrumb">
                { breadcrumbs.map { breadcrumb =>
                  <li>
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
