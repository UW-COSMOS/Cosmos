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





function Histogram(props){
    const r = useStyles()
    return (
      <Paper classNames={r.root}>
        <XYPlot
            xDomain={props.xdomain}
            yDomain={props.ydomain}
            width={1000}
            height={400} >
            <VerticalGridLines />
            <HorizontalGridLines />
            <XAxis title='Values'/>
            <YAxis title='Bin Count'/>
            <VerticalRectSeries data={props.data} style={{stroke: '#fff'}}/>
        </XYPlot>
      </Paper>
    );
  }
  
  export default Histogram;
