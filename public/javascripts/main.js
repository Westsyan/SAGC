/**
 * Created by yz on 2017/6/20.
 */
var html=" "
function linear(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var parameter = $("#line").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            var category = data.maxCategory;
            xLen = category.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    line: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                chart: {
                    plotBorderWidth: 1,
                    zoomType: 'x',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                credits: {
                    enabled: false
                },
                //标题
                title: {
                    text: null
                },
                //x轴
                xAxis: {
                    type: 'category',
                    categories: category,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    min: 0,
                    title: {
                        text: 'Expression (TPM)'
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                legend: {
                    layout: 'vertical',
                    align: 'left',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal',
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '450px',
                            textAlign: 'center'
                        },
                    }],

                },
                //取的数据
                series: data.infos
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });
        }
    });
}

function circLinear(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var parameter = $("#line").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            var category = data.maxCategory;
            xLen = category.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    line: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                chart: {
                    plotBorderWidth: 1,
                    zoomType: 'x',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                credits: {
                    enabled: false
                },
                title: {
                    text: null
                },
                xAxis: {
                    type: 'category',
                    categories: category,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    min: 0,
                    title: {
                        text: 'Expression (RPM)'
                    },
                    plotLines: [{
                        value: 0,
                        width: 1,
                        color: '#808080'
                    }]
                },
                legend: {
                    layout: 'vertical',
                    align: 'left',
                    verticalAlign: 'middle',
                    borderWidth: 0
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal',
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '450px',
                            textAlign: 'center'
                        },
                    }],

                },
                series: data.infos
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });
        }
    });
}

function boxPlot(id, path) {
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    var url = path + "?id=" + id + "";
    $.ajax({
        url: url,
        type: "get",
        dataType: "json",
        success: function (data) {
            $("#charts").highcharts({
                credits: {
                    enabled: false
                },
                chart: {
                    type: 'boxplot',
                    height: 550,
                    marginTop: 40,
                    marginBottom: 80
                },
                title: {
                    text: id
                },
                legend: {
                    enabled: false
                },
                xAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data.tissue,
                    title: {
                        text: 'Groups '
                    }
                },
                yAxis: {
                    title: {
                        text: 'Expression (RPM)'
                    },
                    min: 0
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '90px',
                            top: '475px'
                        }
                    }]
                },
                plotOptions: {
                    boxplot: {
                        fillColor: '#F0F0E0',
                        lineWidth: 2,
                        medianColor: '#0C5DA5',
                        medianWidth: 3,
                        stemColor: '#A63400',
                        stemDashStyle: 'dot',
                        stemWidth: 1,
                        whiskerColor: '#3D9200',
                        whiskerLength: '20%',
                        whiskerWidth: 3
                    }
                },
                series: [{
                    name: id,
                    data: data.ev
                },
                    {
                        name: 'Outlier',
                        color: Highcharts.getOptions().colors[0],
                        type: 'scatter',
                        marker: {
                            fillColor: 'white',
                            lineWidth: 1,
                            lineColor: Highcharts.getOptions().colors[0]
                        },
                        tooltip: {
                            pointFormat: 'RPM: {point.y}'
                        }
                    }]
            });
        }
    });
}


function heatmap(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    parameter = $("#heatmap").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            xLen = data[0].treatment.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    heatmap: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                credits: {
                    enabled: false
                },
                chart: {
                    zoomType: 'xy',
                    type: 'heatmap',
                    height: 550,
                    marginTop: 65,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                title: {
                    text: null
                },
                xAxis: {
                    categories: data[0].treatment,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data[0].gt,
                    title: "Gene"
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '425px'
                        }
                    }]
                },
                colorAxis: {
                    stops: [
                        [0.1, '#78b8ed'],
                        [0.5, '#fffbbc'],
                        [0.8, '#c4463a'],
                        [1, '#c4463a']
                    ],
                    min: 0
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                },
                legend: {
                    title: {
                        style: {
                            fontWeight: '1',
                            color: '#555',
                            fontSize: '12px'
                        },
                        text: 'TPM (log2+1)'
                    },
                    align: 'right',
                    layout: 'vertical',
                    marginTop: 0,
                    verticalAlign: 'top',
                    y: 25,
                    symbolHeight: 305
                },
                tooltip: {
                    formatter: function () {
                        return '<b>Sample Name:</b>' + this.series.xAxis.categories[this.point.x] + '<br>' + this.series.yAxis.categories[this.point.y] + '<br><b>TPM (log2+1): ' + this.point.value + '</b>';
                    }
                },
                series: [{
                    borderWidth: '0.2',
                    color: '#fefefe',
                    data: data[0].expression,
                    dataLabels: {
                        enabled: false
                    }
                }]
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });

        }
    });
}

function circHeatmap(url) {
    var chart = null;
    var maxLabelNum = 30;
    var xLen = 0;
    var step;
    $("#charts").html("<img src='/assets/images/loading.gif'/>");
    parameter = $("#heatmap").data("url");
    $.ajax({
        url: url + "?" + parameter,
        type: "get",
        dataType: "json",
        success: function (data) {
            xLen = data[0].treatment.length;
            step = Math.round(xLen / maxLabelNum) < 1 ? 1 : Math.round(xLen / maxLabelNum);
            $("#charts").highcharts({
                plotOptions: {
                    heatmap: {
                        turboThreshold: 0 //不限制数据点个数
                    }
                },
                credits: {
                    enabled: false
                },
                chart: {
                    zoomType: 'xy',
                    type: 'heatmap',
                    height: 550,
                    marginTop: 65,
                    marginBottom: 170,
                    events: {
                        //监听图表区域选择事件
                        selection: function (event) {//动态修改
                            var len = event.xAxis[0].max - event.xAxis[0].min;
                            var interval = Math.round(len / maxLabelNum < 1 ? 1 : Math.round(len / maxLabelNum));
                            DynamicChangeTickInterval(interval);
                        }
                    }
                },
                title: {
                    text: null
                },
                xAxis: {
                    categories: data[0].treatment,
                    labels: {
                        rotation: -45,
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    tickInterval: step,
                    title: {
                        text: 'Samples'
                    }
                },
                yAxis: {
                    labels: {
                        style: {
                            fontSize: '12px',
                            fontFamily: 'Arial, sans-serif'
                        }
                    },
                    categories: data[0].gt,
                    title: "Gene"
                },
                labels: {
                    style: {
                        color: "black",
                        fontSize: '16px',
                        fontWeight: 'normal'
                    },
                    items: [{
                        html: html,
                        style: {
                            left: '0px',
                            top: '425px'
                        }
                    }]
                },
                colorAxis: {
                    stops: [
                        [0.1, '#78b8ed'],
                        [0.5, '#fffbbc'],
                        [0.8, '#c4463a'],
                        [1, '#c4463a']
                    ],
                    min: 0
//                        minColor: '#FFFFFF'
//                        maxColor: Highcharts.getOptions().colors[0]
                },
                legend: {
                    title: {
                        style: {
                            fontWeight: '1',
                            color: '#555',
                            fontSize: '12px'
                        },
                        text: 'RPM (log2+1)'
                    },
                    align: 'right',
                    layout: 'vertical',
                    marginTop: 0,
                    verticalAlign: 'top',
                    y: 25,
                    symbolHeight: 305
                },
                tooltip: {
                    formatter: function () {
                        return '<b>Sample Name:</b>' + this.series.xAxis.categories[this.point.x] + '<br>' + this.series.yAxis.categories[this.point.y] + '<br><b>RPM (log2+1): ' + this.point.value + '</b>';
                    }
                },
                series: [{
                    borderWidth: '0.2',
                    color: '#fefefe',
                    data: data[0].expression,
                    dataLabels: {
                        enabled: false
                    }
                }]
            }, function (chartObj) {
                //获得图表对象
                chart = chartObj;
            });

        }
    });
}

function DynamicChangeTickInterval(step) {
    chart.xAxis[0].update({
        tickInterval: step
    });
    //扩展或者重写Highcharts图表组件的方法
    ExtendHighcharts();
}

//扩展或者重写Highcharts图表组件的方法
function ExtendHighcharts() {
    Highcharts.extend(Highcharts.Chart.prototype, {
        zoomOut: function () {
            //还原图表X轴的间隔
            DynamicChangeTickInterval(step);
            //还原图表初始状态
            this.zoom();
        }
    });
}