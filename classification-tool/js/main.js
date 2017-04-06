function reportSunroom(currentPin /* string */, buildingType /* string */) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", "/report?pin=" + currentPin + "&building_type=" + buildingType, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.onreadystatechange = function() {
        if (xhr.readyState === 4) {
            console.log(xhr.responseText);
        }
    };
    xhr.send();
}

function next() {
    window.location.href = "/property/" + nextBuilding;
}

function previous() {
    window.location.href = "/property/" + prevBuilding;
}

window.addEventListener('keydown', function(event) {
    var currentUrl = window.location.href;
    var pin = currentUrl.substr(currentUrl.lastIndexOf('/') + 1);


    switch (event.keyCode) {
        case 37: // Left arrow
            previous();
            break;

        case 39: // Right arrow
            next();
            break;

        case 83: // 's'
            reportSunroom(pin, 'Sunroom');
            next();
            break;

        case 66: // 'b'
            reportSunroom(pin, 'BaySunroom');
            next();
            break;

        case 80: // 'p'
            reportSunroom(pin, 'Sunporch');
            next();
            break;

        case 85: // 'u'
            reportSunroom(pin, 'Unknown');
            next();
            break;

        case 87: // 'w'
            reportSunroom(pin, 'BayWindow');
            next();
            break;
    }
});
