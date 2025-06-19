// Real-time clock script
function updateClock() {
    const clockElement = document.getElementById("dynamic-clock");
    if (clockElement) {
      const now = new Date();
      const timeString = now.toLocaleTimeString([], { 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit' 
      });
      clockElement.textContent = timeString;
    }
  }
  
  // Update the clock every second
  setInterval(updateClock, 1000);


  $(document).on('blur', '.format-number', function() {
    var num = $(this).val().replace(/,/g, '');
    if (!isNaN(num) && num !== '') {
      $(this).val(Number(num).toLocaleString());
    }
  });


  // www/js/custom.js
Shiny.addCustomMessageHandler('scrollToElement', function(id) {
  var el = document.getElementById(id);
  if (el) {
    el.scrollIntoView({ behavior: 'smooth' });
  }
});


$(function () {
  // Hide ANY open popover when the click target is
  //   – NOT the icon itself ('.info-icon'), and
  //   – NOT inside an already-open popover ('.popover')
  $('body').on('click', function (e) {
    if (!$(e.target).closest('.popover, .info-icon').length) {
      $('.info-icon').popover('hide');
    }
  });
});

// this runs when R calls session$sendCustomMessage("printPage",…)
Shiny.addCustomMessageHandler("printPage", function(message) {
  window.print();
});

// Add this JavaScript to your app to handle sidebar collapse
// Put this in your tags$script() section or in a separate JS file

$(document).ready(function() {
  // Function to adjust menu container position based on sidebar state
  function adjustMenuContainer() {
    var isCollapsed = $('body').hasClass('sidebar-collapse');
    var menuContainer = $('.menu-container');
    var logos = $('.logos');
    
    if (isCollapsed) {
      // Sidebar is collapsed
      menuContainer.css({
        'left': '0',
        'width': '60px'  // Typical collapsed sidebar width
      });
      
      logos.css({
        'left': '0',
        'width': '60px',
        'padding': '10px 5px'
      });
      
      // Hide text in menu items
      $('.menu-container .nav-link p, .menu-container .nav-link span:not(.fa)').hide();
      
    } else {
      // Sidebar is expanded
      menuContainer.css({
        'left': '0',
        'width': '260px'  // Your normal sidebar width
      });
      
      logos.css({
        'left': '0',
        'width': '260px',
        'padding': '20px 10px 10px 10px'
      });
      
      // Show text in menu items
      $('.menu-container .nav-link p, .menu-container .nav-link span:not(.fa)').show();
    }
  }
  
  // Listen for sidebar toggle button clicks
  $('[data-widget="pushmenu"]').on('click', function() {
    setTimeout(adjustMenuContainer, 300);  // Wait for animation
  });
  
  // Also listen for body class changes
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.attributeName === "class") {
        adjustMenuContainer();
      }
    });
  });
  
  observer.observe(document.body, {
    attributes: true,
    attributeFilter: ['class']
  });
  
  // Initial adjustment
  adjustMenuContainer();
});

