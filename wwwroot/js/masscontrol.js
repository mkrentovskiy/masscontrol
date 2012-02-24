(function($) {


	$.mc = function() { 
		$(document).ajaxError(function() { alertMe("Error!", "Some problems on server. Please, check server logs."); });
		$.getJSON("/nodes_list", function(d) { showNodesList(d); }); 
	}
	
	$.mc.add = function() {
		if(!$.FormCheck.val($("#ahf"))) return;
		$.getJSON("/add_node", $("#ahf").serialize(), function(d) { 
				if(d == "ok") { 
					addNode({ 
							id: $("#ahf_u").val() + "." + $("#ahf_h").val(), 
							host: $("#ahf_h").val(),
							user: $("#ahf_u").val(),
							title: $("#ahf_t").val()
						});
					$("#ahf").clearForm();
				} else {
					alertMe("Error!", d);
				} 
			});
	};
	
	
	$.mc.del = function(id) {
		if(confirm("Are you sure?")) {
			$.getJSON("/del_node", "id=" + id, function(d) { 
					$("#t_" + id2eid(id)).detach(); 
					$("#" + id2eid(id)).detach(); 
					$('#tabs a:last').tab('show');
				});
		}	
	}
	
	$.mc.reconnect = function(id) {
		if(confirm("Are you sure?")) {
			$.getJSON("/reconnect", "id=" + id);
		}	
	}
	$.mc.exec = function(id) {
		var p = $("#command_" + id2eid(id)).val();
		if(p && p.length > 0) {
			exec("command", id, p, "info", function(d) { return strip(d); });
		}
	}
	$.mc.git = function(id) {
		var p = $("#command_" + id2eid(id)).val();
		if(p && p.length > 0) {
			exec("git", id, p, "info", function(d) { return strip(d); });
		}
	}
	$.mc.execme = function(id, c) {
		exec("command", id, c, "info", function(d) { return strip(d); });
	}
	$.mc.shall = function(id) {
		exec("ipsec", id, "", "info", function(d) {
				if(d == "error") {
					alertMe("Error!", "There are some troubles to get sa_mgr info.");
				} else {
					$("#_t_shall").tmpl({"d": d}).appendTo("#info_" + id2eid(id));
					return null;
				}
			});
	}
	
	
	
	/* Local */
	
	function showNodesList(d) {
		$("#tabs").html("");
		$("#pads").html("");
		$.each(d, function(i, v) { addNode(v); });
		$('#tabs a:last').tab('show');
	}
	
	function addNode(v) { 
		v.eid = id2eid(v.id);
		$("#tabs").append($.tmpl($("#_t_tab").html(), v));
		$("#pads").append($.tmpl($("#_t_pad").html(), v));
	}
	
	
	function exec(cmd, id, c, pref, cb) {
		var eid = "#" + pref + "_" + id2eid(id);
		
		$(eid).html($("#_loading").html());
		$.getJSON("/" + cmd + "?id=" + id + "&c=" + escape(c), function(d) { 
				if(cb) d = cb(d); 
				if(d) $(eid).html(d);
			});
	}
	
	
	function alertMe(h, b) { 
		$("#_alert_title").html(h);
		$("#_alert_text").html(b);
		$('#_alert').modal(); 
	}
	
	function id2eid(id) { return id.replace(/\./g, "_"); }
	
	function strip(text) {
		if(text) {
			var a = [["&", "&amp;"], ["<", "&lt;"], [">", "&gt;"],  ["\r\n", "<br/>"],
				["\t", "&nbsp;&nbsp;&nbsp;&nbsp;"], [" ", "&nbsp;"]];
			$(a).each(function(i, v) { var r = new RegExp(v[0], "g"); text = text.replace(r, v[1]); })
		}
		return text;
	}
		
})(jQuery);	
$(document).ready(function() { $.mc(); });