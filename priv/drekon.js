var D1 = {
	"name": "Good day",
	"type": "drakon",
	"background": "", "diaLine": "", "diaLineThickness": "", "font": "14px Arimo",
	"nodes": {
		"1": { "id": "1", "type": "beginend", "isLine": false, "x": 300, "y": 80, "w": 70, "h": 20, "content": { "txt2": "", "txt": "Good day", "font": "bold 16px Arimo" }, "role": "header" },
		"2": { "id": "2", "type": "end", "isLine": false, "x": 300, "y": 320, "w": 40, "h": 20, "content": { "txt": "End", "txt2": "" } },
		"4": { "id": "4", "type": "action", "isLine": false, "x": 300, "y": 140, "w": 60, "h": 20, "content": { "txt": "Eat breakfast", "txt2": "" } },
		"7": { "id": "7", "type": "action", "isLine": false, "x": 300, "y": 200, "w": 60, "h": 20, "content": { "txt": "Eat lunch", "txt2": "" } },
		"10": { "id": "10", "type": "action", "isLine": false, "x": 300, "y": 260, "w": 60, "h": 20, "content": { "txt": "Eat dinner", "txt2": "" } }
	},
	"edges": {
		"5": { "id": "5", "type": "vertical", "isLine": true, "isVertical": true, "head": "1", "tail": "4", "role": "down" },
		"8": { "id": "8", "type": "vertical", "isLine": true, "isVertical": true, "head": "4", "tail": "7", "role": "down" },
		"11": { "id": "11", "type": "vertical", "isLine": true, "isVertical": true, "head": "7", "tail": "10", "role": "down" },
		"12": { "id": "12", "type": "vertical", "isLine": true, "isVertical": true, "head": "10", "tail": "2", "role": "down" }
	}
};

var D = {
	"name": "Good day",
	"type": "drakon",
	"background": "", "diaLine": "", "diaLineThickness": "", "font": "14px Arimo",
	"nodes": {
		"1": { "id": "1", "type": "beginend", "isLine": false, "x": 310, "y": 120, "w": 70, "h": 20, "content": { "txt2": "", "txt": "Good day", "font": "bold 16px Arimo" }, "role": "header" },
		"2": { "id": "2", "type": "end", "isLine": false, "x": 310, "y": 260, "w": 40, "h": 20, "content": { "txt": "End", "txt2": "" } },
		"16": { "id": "16", "type": "question", "isLine": false, "x": 310, "y": 180, "w": 120, "h": 20, "content": { "txt": "test?", "txt2": "" }, "flag1": 1 },
		"17": { "id": "17", "type": "junction", "isLine": false, "x": 470, "y": 180, "w": 0, "h": 0 },
		"18": { "id": "18", "type": "junction", "isLine": false, "x": 470, "y": 220, "w": 0, "h": 0 },
		"19": { "id": "19", "type": "junction", "isLine": false, "x": 310, "y": 220, "w": 0, "h": 0 }
	},
	"edges": {
		"20": { "id": "20", "type": "horizontal", "isLine": true, "isVertical": false, "head": "16", "tail": "17" },
		"21": { "id": "21", "type": "vertical", "isLine": true, "isVertical": true, "head": "17", "tail": "18", "role": "down" },
		"22": { "id": "22", "type": "horizontal", "isLine": true, "isVertical": false, "head": "19", "tail": "18", "role": "left" },
		"23": { "id": "23", "type": "vertical", "isLine": true, "isVertical": true, "head": "16", "tail": "19", "role": "down" },
		"24": { "id": "24", "type": "vertical", "isLine": true, "isVertical": true, "head": "1", "tail": "16", "role": "down" },
		"25": { "id": "25", "type": "vertical", "isLine": true, "isVertical": true, "head": "19", "tail": "2", "role": "down" }
	}
};

$(function () {
	$('#title').text(D.name);
	var draw = SVG('drawing').size(400, 300);

	var nodes = D.nodes;
	for (id in nodes) {
		var node = nodes[id];
		switch (node.type) {
			case "beginend":
			case "end":
				draw.rect(node.w, node.h).attr({ fill: '#f05' }).radius(10).move(node.x, node.y);
				break;
			case "question":
				draw.polygon(
					(node.x + node.h / 2) + ',' + node.y + ' ' +
					(node.x + node.w - node.h / 2) + ',' + node.y + ' ' +
					(node.x + node.w) + ',' + (node.y + node.h / 2) + ' ' +
					(node.x + node.w - node.h / 2) + ',' + (node.y + node.h) + ' ' +
					(node.x + node.h / 2) + ',' + (node.y + node.h) + ' ' +
					(node.x) + ',' + (node.y + node.h / 2)).attr({ fill: '#f05' });
				break;
			default:
				draw.rect(node.w, node.h).attr({ fill: '#f06' }).move(node.x, node.y);
				break;
		}
	}

	var edges = D.edges;
	for (id in edges) {
		var edge = edges[id];
		switch (edge.type) {
			case "vertical":
				draw.line(nodes[edge.head].x + nodes[edge.head].w / 2, nodes[edge.head].y + nodes[edge.head].h,
					nodes[edge.tail].x + nodes[edge.tail].w / 2, nodes[edge.tail].y)
					.stroke({ color: '#f06', width: 2, linecap: 'round' });
				break;
			case "horizontal":
				draw.line(nodes[edge.head].x + nodes[edge.head].w, nodes[edge.head].y + nodes[edge.tail].h / 2,
					nodes[edge.tail].x, nodes[edge.tail].y + nodes[edge.tail].h / 2)
					.stroke({ color: '#f06', width: 2, linecap: 'round' });
				break;
		}
		/*$('#log').html($('#log').html()+'line ('+nodes[edge.head].x+','+nodes[edge.head].y+') -> ('+nodes[edge.tail].x+','+ nodes[edge.tail].y+')<br>');*/
	}
});