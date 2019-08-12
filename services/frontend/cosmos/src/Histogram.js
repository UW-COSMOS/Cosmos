import React, { Component } from 'react';
import { makeStyles } from '@material-ui/core/styles';
import '../node_modules/react-vis/dist/style.css';
import {XYPlot, LineSeriesXYPlot,
    XAxis,
    YAxis,
    VerticalGridLines,
    HorizontalGridLines,
    VerticalRectSeries} from 'react-vis';
import Paper from '@material-ui/core/Paper';

const useStyles = makeStyles(theme => ({
  root: {
    display: 'flex',
    alignItems: 'center',
    width: "50%"
  },
}));


const DATA = [
    {x0: 0, x: 1, y: 1},
    {x0: 1, x: 2, y: 1},
    {x0: 3, x: 4, y: 1},
    {x0: 4, x: 5, y: 2},
    {x0: 5, x: 6, y: 2.2},
    {x0: 6, x: 7, y: 1},
    {x0: 7, x: 8, y: 2.5},
    {x0: 8, x: 9, y: 1}
  ];

function Histogram(){
    const r = useStyles()
    return (
      <Paper classNames={r.root}>
        <XYPlot
            xDomain={[0,9]}
            yDomain={[0,3]}
            xType="time"
            width={1000}
            height={400} >
            <VerticalGridLines />
            <HorizontalGridLines />
            <XAxis title='TOC Values'/>
            <YAxis title='Frequency'/>
            <VerticalRectSeries data={DATA} style={{stroke: '#fff'}}/>
        </XYPlot>
      </Paper>
    );
  }
  
  export default Histogram;
