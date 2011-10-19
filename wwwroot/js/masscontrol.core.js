(function($) {

	$.mc = function() { $.getJSON("/nodes_list", function(d) { showNodesList(d); }); }
	
	$.mc.addNode = function() {
		if(!$.FormCheck.val($("#ahf"))) return;
		$.getJSON("/add_node", $("#ahf").serialize(), function(d) { 
				if(d == "ok") { 
					addNodeView({ 
							id: $("#h_u").val() + "." + $("#h_h").val(), 
							host: $("#h_h").val(),
							user: $("#h_u").val(),
							type: $("#h_t").val()							 
						});
					$("#ahf").clearForm();
				} else {
					alert("Error! " + d);
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
		if($(e).css('display', 'none')) $(e).show(); 
		else $(e).hide(); 
	}
	
	$.mc.exec = function(id, command) {
		
	}
	
	$.mc.git = function(id, command) {
	
	}
	
	function addToHistory(id, eid, command) {
		
	}
	
	
	function showNodesList(d) {
		$("#hl").html("");
		$.each(d, function(i, v) { addNodeView(v); });
	}	
	
	function addNodeView(v) { 
		v.eid = id2eid(v.id); 
		$("#hl").append($.tmpl($("#_t_host_" + v.type).html(), v)); 
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