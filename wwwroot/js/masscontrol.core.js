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
			$.getJSON("/send_command", "id=" + id + "&c=" + escape(c), function(d) { 
					$("#r_" + id2eid(id)).show().html(strip(d)); 
				});
		}	
		return false;
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