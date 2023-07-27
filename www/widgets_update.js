// Function to move labels
function moveLabels() {
    const dropdowns = document.querySelectorAll('.dropdown.bootstrap-select.form-control.bs3');
    dropdowns.forEach(dropdown => {
        const grandParent = dropdown.parentNode.parentNode;
        const label = grandParent.querySelector('.control-label');

        // If the label is not found or it has 'shiny-label-null' class, adjust button and return
        if (!label || label.classList.contains('shiny-label-null')) {
            const button = grandParent.querySelector('.btn.dropdown-toggle.btn-default');
            if (button) button.style.top = '0px';
            dropdown.style.height = "auto";
            return;
        }

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

// Function to adjust grid margin/height for sliders that do NOT have a grid under them
function adjustGridMargin() {
    const irsElements = document.querySelectorAll('.irs--shiny');
    irsElements.forEach(irs => {
        const gridElement = irs.querySelector('.irs-grid');

        // If the grid element doesn't exist or is empty, remove margin-bottom style
        if (!gridElement || gridElement.innerHTML.trim() === '') {
            irs.style.height = '40px';
        }
    });
}

// Function to hide bottom_sidebar if its content is hidden or empty
function hideBottomSidebarIfEmptyOrHidden() {
    const bottomSidebars = document.querySelectorAll('.bottom_sidebar');

    bottomSidebars.forEach(bottomSidebar => {
        // get the children elements of bottom_sidebar
        const children = bottomSidebar.children;

        let shouldHide = true; // flag to check if bottom_sidebar should be hidden

        // loop through each child element
        for(let i = 0; i < children.length; i++) {
            const child = children[i];

            // if the child element is not hidden and is not empty, don't hide bottom_sidebar
            if(child.style.display !== 'none' && child.innerHTML.trim() !== '') {
                shouldHide = false;
                break;
            }
        }

        // if shouldHide is still true after checking all children, hide bottom_sidebar
        if(shouldHide) {
            bottomSidebar.style.display = 'none';
        } else {
            bottomSidebar.style.display = '';
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
                // When a childList or attributes mutation occurs, try to move labels and adjust grid margins immediately
                moveLabels();
                adjustGridMargin();
                hideBottomSidebarIfEmptyOrHidden(); // call the new function here
            }
        }
    };

    // Create an observer instance linked to the callback function
    let observer = new MutationObserver(callback);

    // Start observing the target node for configured mutations
    observer.observe(targetNode, config);
});