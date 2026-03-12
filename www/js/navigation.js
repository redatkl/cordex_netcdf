  // ── State ─────────────────────────────────────────────────
  $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'current_page') {
        $('.nav-btn').removeClass('active');
        $('#nav_' + event.value).addClass('active');
      }
    });