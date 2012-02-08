(function($) {
	$.mc = function() { $.getJSON("/nodes_list", function(d) { showNodesList(d); }); }
	
	$.mc.addNode = function() {
		if(!$.FormCheck.val($("#ahf"))) return;
		$.getJSON("/add_node", $("#ahf").serialize(), function(d) { 
				if(d == "ok") { 
					addNodeView({ 
							id: $("#ahf_u").val() + "." + $("#ahf_h").val(), 
							host: $("#ahf_h").val(),
							user: $("#ahf_u").val()
						});
					$("#ahf").clearForm();
				} else {
					alertMe("Error!", d);
				} 
			});
	};
	
	
	$.mc.delNode = function(id) {
		if(confirm("Are you sure?")) {
			$.getJSON("/del_node", "id=" + id, function(d) { 
					$("#i_" + id2eid(id)).detach(); 
				});
		}	
	}
	
	$.mc.sendCommand = function(id) {
		var c = $("#cli_" + id2eid(id)).val(); 
	
		if(c && c.length && c.length > 0) {
			var eid = id2eid(id);
			var t = $("#r_" + eid).show().html("<img src='/i/load.gif' alt='Wait!' title='Wait!'/>");	
			
			addToHistory(id, eid, c);
										
			$.getJSON("/send_command", "id=" + id + "&c=" + escape(c), function(d) { 
					$("#r_" + eid).html(strip(d)); 
				});
		}	
		return false;
	}
	
	$.mc.showHideHistory = function(id) {
		var e = "#h_" + id2eid(id);
		if($(e).css('display') == 'none') $(e).show(); 
		else $(e).hide(); 
	}
	
	$.mc.exec = function(id, command) {
		var eid = id2eid(id);
		var c = command.replace('\"', '"');
	
		$("#cli_" + eid).val("").val(c);
		$.mc.sendCommand(id);			
	}
	
	$.mc.git = function(id, command) {
		var eid = id2eid(id);
		var c = escape(command.replace('\"', '"'));
		
		var t = $("#r_" + eid).show().html("<img src='/i/load.gif' alt='Wait!' title='Wait!'/>");	
		$.getJSON("/add_to_git", "id=" + id + "&c=" + c, function(d) { 
				$("#r_" + eid).html(strip(d)); 
			});		
	}
	
	function addToHistory(id, eid, command) {
		var c = command.replace('"', '\"');
		var sc = strip(command);
		var d = { 'id': id, 'eid': eid, 'command': c, 'scommand': sc };		
		var m = true;
		
		$("#h_" + eid + " .hi").each(function(i, v) { 
				if($(v).attr('ref') == c) m = false; 
			});
		if(m) $("#h_" + eid).show().prepend($.tmpl($("#_t_history").html(), d))
	}
	
	
	function showNodesList(d) {
		$("#tabs").html("");
		$("#pads").html("");
		$.each(d, function(i, v) { addNodeView(v); });
		$('#tabs a:first').tab('show');
	}
	
	function addNodeView(v) { 
		v.eid = id2eid(v.id);
		$("#tabs").append($.tmpl($("#_t_tab").html(), v));
		$("#pads").append($.tmpl($("#_t_pad").html(), v));
		updateSysInfo(v, true);
	}
	
	function updateSysInfo(v, n) {
	}
	
	function alertMe(h, b) { 
		$("#alert_header").html(h);
		$("#alert_body").html(b);
		$('#alert').modal(); 
	}
	
	function id2eid(id) { return id.replace(".", "_", "g"); }
	
	function strip(text) {
	    if(text) {
	    	var a = [["&", "&amp;"], ["<", "&lt;"], [">", "&gt;"],  ["\n", "<br/>"],
	    		 ["\t", "&nbsp;&nbsp;&nbsp;&nbsp;"], [" ", "&nbsp;"],	    		 
	    		];
			$(a).each(function(i, v) { var r = new RegExp(v[0], "g"); text = text.replace(r, v[1]); })
	    }
		return text;
	}
		
})(jQuery);	
$(document).ready(function() { $.mc(); });