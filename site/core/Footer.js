/**
 * Copyright (c) 2017-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

const React = require('react');

const highlighterCode = `
function fn() {
  Array.prototype.forEach.call(
    document.querySelectorAll("pre"),
    hljs.highlightBlock
  );
};
if (document.attachEvent ? document.readyState === "complete" : document.readyState !== "loading"){
  fn();
} else {
  document.addEventListener('DOMContentLoaded', fn);
}
`;

class Footer extends React.Component {
  render() {
    const currentYear = new Date().getFullYear();
    return (
      <footer className="nav-footer" id="footer">
        <section className="sitemap">
          <a href={this.props.config.baseUrl} className="nav-home">
            <img
              src={this.props.config.baseUrl + this.props.config.headerIcon}
              alt={this.props.config.title}
              width="66"
              height="58"
            />
          </a>
          <div>
            <h5>Docs</h5>
            <a
              href={
                this.props.config.baseUrl +
                'docs/' +
                this.props.language +
                '/getting-started.html'
              }>
              Getting Started
            </a>
            <a
              href={
                this.props.config.baseUrl +
                'docs/' +
                this.props.language +
                '/configuration.html'
              }>
              Project Configuration
            </a>
            <a
              href={
                this.props.config.baseUrl +
                'docs/' +
                this.props.language +
                '/commands.html'
              }>
              Commands Reference
            </a>
          </div>
          <div>
            <h5>Community</h5>
            <a href="https://discord.gg/reasonml">Discord</a>
            <a href="http://stackoverflow.com/questions/tagged/esy">Stack Overflow</a>
          </div>
        </section>
      </footer>
    );
  }
}

module.exports = Footer;
