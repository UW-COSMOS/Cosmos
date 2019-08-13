import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Grid from '@material-ui/core/Grid';
import GridListTile from '@material-ui/core/GridListTile';
import CodeCard from './CodeCard.js'
import CircularProgress from '@material-ui/core/CircularProgress';
import Hidden from '@material-ui/core/Hidden';

const useStyles = makeStyles(theme => ({
  gridList: {
    flexGrow: 1
  }
}));




export default function CodeGrid(props){
    const classes = useStyles();
    function compute_tile(tile, i){
      return (<Grid key={i} xs={12} item>
                <CodeCard data={tile} handleClick={props.handleClick}></CodeCard>
              </Grid>)
    }
    
    function compute_grid(data){
      return data.map(compute_tile)
    }
    return (<div>
    <Hidden xlDown={props.data.length != 0}>
      <CircularProgress color="secondary" />
    </Hidden>
    <Grid container className={classes.grid} justify="center" align="center" spacing={2}>
      {compute_grid(props.data)}
    </Grid>
    </div>)
}
