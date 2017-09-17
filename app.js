var path = require('path')
var config = require('./config.json')
var express = require('express')
var session = require('express-session')
var app = express()

app.set('views', path.join(__dirname, 'views'))
app.set('view engine', 'ejs')
app.use(express.static(path.join(__dirname, 'public')))
app.use(session({secret: 'secret', resave: 'false', saveUninitialized: 'false'}))

// Initial view - loads Connect To QuickBooks Button
app.get('/', function (req, res) {
  res.render('home', config)
});

app.get('/search', function (req, res) {
  res.render('search', { tab: 'search' })
});

app.get('/florist', function (req, res) {
    res.render('otherbiz')
});

app.get('/evaluation', function (req, res) {
    res.render('chat')
});

// Sign In With Intuit, Connect To QuickBooks, or Get App Now
// These calls will redirect to Intuit's authorization flow
app.use('/sign_in_with_intuit', require('./routes/sign_in_with_intuit.js'))
app.use('/connect_to_quickbooks', require('./routes/connect_to_quickbooks.js'))
app.use('/connect_handler', require('./routes/connect_handler.js'))

// Callback - called via redirect_uri after authorization
app.use('/callback', require('./routes/callback.js'))

// Connected - call OpenID and render connected view
app.use('/connected', require('./routes/connected.js'))

// Call an example API over OAuth2
app.use('/api_call', require('./routes/api_call.js'));
app.use('/creditage', require('./routes/creditage.js'));
app.use('/unpaidar', require('./routes/unpaidar.js'));
app.use('/customers', require('./routes/customers.js'));
// Start server on HTTP (will use ngrok for HTTPS forwarding)
app.listen(3000, function () {
  console.log('Example app listening on port 3000!')
})
