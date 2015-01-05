describe('send', function(){
  it('should refresh after field change', function(done) {
    // given
    var sendFormFn = function sendForm(formValue, renderResponseFn, sendErrorFn, isAction, triggeringElement) {
      renderResponseFn(simple1.form2);

      // then
      byName('field1').val().should.equal('v1');
      byName('field2').val().should.equal('');
      byName('field3').val().should.equal('15');

      done();
    };

    var sf = new SuplerForm(container, {
      send_form_function: sendFormFn
    });

    // when
    sf.render(simple1.form1);
    byName('field1').change();

    // then in callback
  });

  it('should apply results of the last refresh started only', function() {
    // given
    var state = 1;
    var renderResponseFn1 = null;
    var renderResponseFn2 = null;

    function sendForm(formValue, renderResponseFn, sendErrorFn, isAction, triggeringElement) {
      if (state === 1) {
        // we pretend the request is taking some time, so we are just putting the renderResponseFn aside and allow
        // the test to continue
        renderResponseFn1 = renderResponseFn;
        state = 2;
      } else if (state === 2) {
        renderResponseFn2 = renderResponseFn;
        state = 3;
      } else {
        assert.fail(0, state, 'Send called in an illegal state');
      }
    }

    var sf = new SuplerForm(container, {
      send_form_function: sendForm
    });

    // when & then
    sf.render(simple1.form1);
    byName('field3').val(20);

    byName('field3').change();
    state.should.equal(2);

    byName('field3').change();
    state.should.equal(3);

    // first request completes, but another is started -> results should not be applied
    renderResponseFn1(simple1.form2);
    byName('field3').val().should.not.equal('15');

    // second request completes, results should be applied
    renderResponseFn2(simple1.form2);
    byName('field3').val().should.equal('15');
  });

  it('should drop refreshes when an action is in progress', function() {
    // given
    var state = 1;
    var actionRenderResponseFn = null;

    function sendForm(formValue, renderResponseFn, sendErrorFn, isAction, triggeringElement) {
      if (state === 1) {
        actionRenderResponseFn = renderResponseFn;
        state = 2;
      } else {
        assert.fail(1, state, 'The refresh should have been dropped!');
      }
    }

    var sf = new SuplerForm(container, {
      send_form_function: sendForm
    });

    // when & then
    sf.render(simple1action.form1);

    byName('inc').click();
    state.should.equal(2);

    byName('field3').val(1);
    byName('field3').change();
    state = 3;

    // first request completes, but another is started -> results should not be applied
    actionRenderResponseFn(simple1action.form2);
    byName('field3').val().should.equal('15');
  });

  it('should enqueue actions when an action is in progress', function() {
    // given
    var state = 1;
    var actionRenderResponseFn1 = null;
    var actionRenderResponseFn2 = null;

    function sendForm(formValue, renderResponseFn, sendErrorFn, isAction, triggeringElement) {
      if (state === 1) {
        actionRenderResponseFn1 = renderResponseFn;
        state = 2;
      } else if (state === 2) {
        assert.fail(0, state, 'The action should have been enqueued!');
      } else if (state === 3) {
        actionRenderResponseFn2 = renderResponseFn;
        state = 4;

        // the form value should include the changes from the first action
        formValue.field3.should.equal(15);
      } else {
        assert.fail(0, state, 'Send called in an illegal state');
      }
    }

    var sf = new SuplerForm(container, {
      send_form_function: sendForm
    });

    // when & then
    sf.render(simple1action.form1);

    byName('inc').click();
    state.should.equal(2);

    byName('inc').click();
    state = 3;

    // first action completes, second should be started.
    actionRenderResponseFn1(simple1action.form2);
    state.should.equal(4);

    actionRenderResponseFn2(simple1action.form1);
    byName('field3').val().should.equal('0');
  });

  it('should still work after an error', function() {
    // given
    var state = 1;
    var sendErrorFn1 = null;
    var renderResponseFn2 = null;

    function sendForm(formValue, renderResponseFn, sendErrorFn, isAction, triggeringElement) {
      if (state === 1) {
        sendErrorFn1 = sendErrorFn;
        state = 2;
      } else if (state === 2) {
        renderResponseFn2 = renderResponseFn;
        state = 3;
      } else {
        assert.fail(0, state, 'Send called in an illegal state');
      }
    }

    var sf = new SuplerForm(container, {
      send_form_function: sendForm
    });

    // when & then
    sf.render(simple1.form1);
    byName('field3').val(20);

    byName('field3').change();
    state.should.equal(2);
    sendErrorFn1();

    byName('field3').change();
    state.should.equal(3);
    renderResponseFn2(simple1.form2);

    byName('field3').val().should.equal('15');
  });
});
