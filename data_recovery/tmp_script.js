// template.js
// THIS IS A TEMPLATE FILE! 
// Placeholders must be replaced with working values before being usable.
var casper = require('casper').create();
casper.start('https://masks4canada.org/2020/12/24/covid19-school-report-wecdsb-grand-erie-dec-23-20200/'); 
casper.wait(3000);
casper.then(function(){
		var html = this.getHTML()
		this.echo(html)
});
casper.run();