let map;
let markers = [];


//The purpose of this block is to load the openstreetmap and markers
//after the HTML content is already on the page
document.addEventListener("DOMContentLoaded", () => {
    console.log("Map check");

    map = L.map('map').setView([41, -74], 13);

    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution:
            '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);

    loadPlaces();
});


const addPlace = async () => {
    const label = document.querySelector("#label").value;
    const address = document.querySelector("#address").value;

    await axios.put('/places', { label: label, address: address});
    await loadPlaces();
}

const deletePlace = async (id) => {
    await axios.delete(`/places/${id}`);
    await loadPlaces();
}

const loadPlaces = async () => {
    const response = await axios.get('/places');
    const tbody = document.querySelector('tbody');
    while (tbody.firstChild) {
        tbody.removeChild(tbody.firstChild);
    }

    for (const marker of markers) {
        map.removeLayer(marker);
    }
    markers = [];

    if (response && response.data && response.data.places) {
        for (const place of response.data.places) {
            const tr = document.createElement('tr');
            tr.innerHTML = `
                <td>${place.label}</td>
                <td>${place.address}</td>
                <td>
                    <button class='btn btn-danger' onclick='deletePlace(${place.id})'>Delete</button>
                </td>
            `;

            if (place.lat != null && place.lng != null) {
                tr.dataset.lat = place.lat;
                tr.dataset.lng = place.lng;
                tr.onclick = on_row_click;
            }

            tbody.appendChild(tr);

            if (place.lat != null && place.lng != null) {
                const marker = L.marker([place.lat, place.lng]).addTo(map).bindPopup(`<b>${place.label}</b><br/>${place.address}`);
                markers.push(marker);
            }
        }
    }
}

const on_row_click = (e) => {
    let row = e.target;
    if (e.target.tagName.toUpperCase() === 'TD') {
        row = e.target.parentNode;
    }
    
    const lat = parseFloat(row.dataset.lat);
    const lng = parseFloat(row.dataset.lng);

    if (!isNaN(lat) && !isNaN(lng)) {
        map.flyTo(new L.LatLng(lat, lng));
    }
};
