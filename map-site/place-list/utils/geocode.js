/*
Adding geocoder gave me the most trouble by a long shot
geocode.js and its method geocodeAddress were made to contain every interaction with openstreetmap to a single file
This was to help me visualize it better
/routes/places.js has this line of code to have it be used by the put function
    const geocodeAddress = require('../utils/geocode');
I used npm install node-fetch@2 to use fetch and nodeFetch functions
*/
const NodeGeocoder = require('node-geocoder');
const nodeFetch = require('node-fetch');

const options = {
    provider: 'openstreetmap',
    httpAdapter: 'https',
    fetch: function fetch(url, options) {
        return nodeFetch(url, {
            ...options,
            headers: {
                ...options.headers,
                'User-Agent': 'my-app/1.0 (jnovack1@ramapo.edu)',
                'Referer': 'http://localhost:8080/'
            }
        });
    }

};

const geocoder = NodeGeocoder(options);

async function geocodeAddress(address) {
    try {
        const res = await geocoder.geocode(address);

        if (!res || res.length === 0) return null;

        const { formattedAddress, latitude, longitude } = res[0];

        return {
            formatted: formattedAddress,
            latitude,
            longitude
        };
    } catch (err) {
        throw new Error('Geocoding Error: ' + err.message);
    }
}

module.exports = geocodeAddress;
