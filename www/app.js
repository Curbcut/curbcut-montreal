// Function to move labels
function moveLabels() {
    const dropdowns = document.querySelectorAll('.dropdown.bootstrap-select.form-control.bs3');
    dropdowns.forEach(dropdown => {
        const grandParent = dropdown.parentNode.parentNode;
        const label = grandParent.querySelector('.control-label');

        // If the label is not found or it has 'shiny-label-null' class, skip the current loop
        if (!label || label.classList.contains('shiny-label-null')) return;

        // If a cloned label already exists, skip the current loop
        if (dropdown.querySelector('.cloned-label')) return;

        // Create a clone of the label and append it to the dropdown
        try {
            const clonedLabel = label.cloneNode(true);
            clonedLabel.classList.add('cloned-label');
            clonedLabel.style.display = 'inherit'; // Ensure the cloned label is visible
            dropdown.appendChild(clonedLabel);

            // Hide the original label
            label.style.display = 'none';
        } catch (e) {
            console.log('Label could not be cloned and moved:', e);
        }
    });
}

document.addEventListener("DOMContentLoaded", function() {
    // The node to be monitored
    let targetNode = document.body; // Adjust this if necessary

    // Options for the observer (which mutations to observe)
    let config = { childList: true, subtree: true, attributes: true };

    // Callback function to execute when mutations are observed
    let callback = function(mutationsList) {
        for(let mutation of mutationsList) {
            if (mutation.type === 'childList' || mutation.type === 'attributes') {
                // When a childList or attributes mutation occurs, try to move labels immediately
                moveLabels();
            }
        }
    };

    // Create an observer instance linked to the callback function
    let observer = new MutationObserver(callback);

    // Start observing the target node for configured mutations
    observer.observe(targetNode, config);
});
