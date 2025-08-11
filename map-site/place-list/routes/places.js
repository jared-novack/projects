const express = require('express');
const router = express.Router();
const geocodeAddress = require('../utils/geocode');

router.get('/', async (req, res) => {
    const places = await req.db.findPlaces();
    res.json({ places });
});

router.put('/', async (req, res) => {

    if (!req.body.label || !req.body.address) {
        return res.status(400).json({ error: 'Label and address are required' });
    }

    try {
        const location = await geocodeAddress(req.body.address);

        if (!location) {
            return res.status(404).json({ error: 'Address not found' });
        }

        const id = await req.db.createPlace(req.body.label, location.formatted, location.latitude, location.longitude);

        res.json({
            id: id,
            label: req.body.label,
            address: location.formatted,
            lat: location.latitude,
            lng: location.longitude
        });

    } catch (error) {
        console.error('Geocoding Error:', error.message);
        res.status(500).json({ error: 'Geocoding or database error' });
    }
});

router.delete('/:id', async (req, res) => {
    await req.db.deletePlace(req.params.id);
    res.status(200).send();
});

module.exports = router;
