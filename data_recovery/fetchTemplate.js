// template.js
// THIS IS A TEMPLATE FILE! 
// Placeholders must be replaced with working values before being usable.
var casper = require('casper').create();
casper.start('${url}'); 
casper.wait(3000);
casper.then(function(){
		var html = this.getHTML()
		this.echo(html)
});
casper.run();
