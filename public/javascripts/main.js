(function() {
  var disableForm, enableForm, scrollWithAnimation;

  scrollWithAnimation = function($id, duration) {
    return $('html,body').animate({
      scrollTop: $id.offset().top - 55
    }, duration);
  };

  disableForm = function($form) {
    var $formGroups;
    $formGroups = $form.find('.form-group:not(.always-editable)');
    $formGroups.find('input:not([type="file"], [type="checkbox"], [type="radio"], [type="hidden"])').removeAttr('disabled').attr('readonly', true);
    $formGroups.find('input[type="file"], input[type="checkbox"], input[type="radio"], select').attr('disabled', true);
    $formGroups.find('.checkbox, .radio, .radio-inline').addClass('disabled');
    return $formGroups.find('.checkbox-group, .radio-group, .select-group').find('input[type="hidden"]').removeAttr('disabled readonly');
  };

  enableForm = function($form) {
    var $formGroups;
    $formGroups = $form.find('.form-group:not(.always-editable)');
    $formGroups.find('input:not([type="file"], [type="checkbox"], [type="radio"], [type="hidden"])').removeAttr('disabled readonly');
    $formGroups.find('input[type="file"], input[type="checkbox"], input[type="radio"], select').removeAttr('disabled readonly');
    $formGroups.find('.checkbox, .radio, .radio-inline').removeClass('disabled');
    return $formGroups.find('.checkbox-group, .radio-group, .select-group').find('input[type="hidden"]').removeAttr('readonly').attr('disabled', true);
  };

  $(function() {
    var $data, checkbox, getParam, hash, p, params, radio, select, text;
    $('.btn-readonly-unlock').click(function(e) {
      if ($(this).hasClass('locked')) {
        $(this).removeClass('locked btn-primary').addClass('btn-danger').text('Lock readonly fields');
        return enableForm($('#form-readonly'));
      } else {
        $(this).removeClass('btn-danger').addClass('locked btn-primary').text('Unlock readonly fields');
        return disableForm($('#form-readonly'));
      }
    });
    if (/\/readonly\/?\?\w/.test(window.location.href)) {
      params = window.location.href.split('?')[1].split('&');
      params = (function() {
        var i, len, results;
        results = [];
        for (i = 0, len = params.length; i < len; i++) {
          p = params[i];
          results.push(p.split('='));
        }
        return results;
      })();
      getParam = function(name, params) {
        var foundParams;
        foundParams = (function() {
          var i, len, results;
          results = [];
          for (i = 0, len = params.length; i < len; i++) {
            p = params[i];
            if (p[0] === name) {
              results.push(p);
            }
          }
          return results;
        })();
        if (foundParams.length > 0) {
          return foundParams[0][1];
        } else {
          return void 0;
        }
      };
      text = getParam("text", params);
      checkbox = getParam("checkbox", params);
      radio = getParam("radio", params);
      select = getParam("select", params);
      if ((text != null) || (checkbox != null) || (radio != null) || (select != null)) {
        $data = $('#bound-data');
        $data.find('#data-text').text(text);
        $data.find('#data-checkbox').text(checkbox);
        $data.find('#data-radio').text(radio);
        $data.find('#data-select').text(select);
        $data.removeAttr('hidden');
      }
    }
    $('.checkbox-group input[type="checkbox"]').change(function() {
      return $(this).parents('.checkbox-group').find('input[type="hidden"]').val($(this).prop('checked'));
    });
    $('.radio-group input[type="radio"]').change(function() {
      var $radioGroup;
      $radioGroup = $(this).parents('.radio-group');
      return $radioGroup.find('input[type="hidden"]').val($radioGroup.find('input[type="radio"]:checked').val());
    });
    $('.select-group select').change(function() {
      return $(this).parents('.select-group').find('input[type="hidden"]').val($(this).val());
    });
    $('.input-daterange').datepicker({
      format: "dd-mm-yyyy",
      todayBtn: "linked",
      todayHighlight: true
    });
    $('body[tab="docs"]').scrollspy({
      target: '#sidebar',
      offset: 60
    });
    $('a[href*="#"]:not([href="#"], [href*="#collapse"], [data-toggle])').click(function(e) {
      var target;
      if (location.pathname.replace(/^\//, '') === this.pathname.replace(/^\//, '') && location.hostname === this.hostname) {
        target = $(this.hash);
        target = target.length ? target : $('[name=' + this.hash.slice(1)(+']'));
        if (target.length) {
          return scrollWithAnimation(target, 500);
        }
      }
    });
    hash = location.hash;
    if (hash.length > 0) {
      scrollWithAnimation($(hash), 10);
    }
    $('.apply-tweak').click(function(e) {
      if ($(this).hasClass('active')) {
        $('.form-inline').removeClass('align-top');
        return $(this).removeClass('btn-danger').addClass('btn-info');
      } else {
        $('.form-inline').addClass('align-top');
        return $(this).removeClass('btn-info').addClass('btn-danger');
      }
    });
    $('.input-number-plus').click(function(e) {
      var $input, current;
      $input = $(this).parent().find('input');
      current = parseInt($input.val(), 10);
      return $input.val(current + 1);
    });
    return $('.input-number-minus').click(function(e) {
      var $input, current;
      $input = $(this).parent().find('input');
      current = parseInt($input.val(), 10);
      return $input.val(current - 1);
    });
  });

}).call(this);