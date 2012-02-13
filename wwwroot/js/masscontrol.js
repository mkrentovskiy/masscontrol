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
					$("#p_" + id2eid(id)).detach(); 
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
			exec("command", id, p, "info", null);
		}
	}
	$.mc.git = function(id) {
		var p = $("#command_" + id2eid(id)).val();
		if(p && p.length > 0) {
			exec("git", id, p, "info", null);
		}
	}
	$.mc.execme = function(id, c) {
		exec("command", id, c, "info", null);
	}
	$.mc.shall = function(id) {
		exec("ipsec", id, "", "info", function(d) { return d; });
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
		updateSysInfo(v);
	}
	
	function updateSysInfo(v) {
		exec("command", v.id, "uname -a;uptime", "sysinfo", function(d) {
				if($("#sysinfo_" + v.eid) && $("#sysinfo_" + v.eid).length > 0) 
					setTimeout("updateSysInfo(v)", 600000);
				return d;
			});
	}
	
	function exec(cmd, id, c, pref, cb) {
		var eid = "#" + pref + "_" + id2eid(id);
		
		$(eid).html($("#_loading").html());
		$.getJSON("/" + cmd + "?id=" + id + "&c=" + escape(c), function(d) { 
				if(cb) d = cb(d); 
				$(eid).html("<span class='c'>" + c + "</span>"  + strip(d));
			});
	}
	
	
	function alertMe(h, b) { 
		$("#_alert_title").html(h);
		$("#_alert_text").html(b);
		$('#_alert').modal(); 
	}
	
	function id2eid(id) { return id.replace(".", "_", "g"); }
	
	function strip(text) {
		if(text) {
			var a = [["&", "&amp;"], ["<", "&lt;"], [">", "&gt;"],  ["\n", "<br/>"],
				["\t", "&nbsp;&nbsp;&nbsp;&nbsp;"], [" ", "&nbsp;"]];
			$(a).each(function(i, v) { var r = new RegExp(v[0], "g"); text = text.replace(r, v[1]); })
		}
		return text;
	}
		
})(jQuery);	
$(document).ready(function() { $.mc(); });