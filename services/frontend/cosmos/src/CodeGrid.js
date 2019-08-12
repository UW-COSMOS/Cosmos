import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import GridList from '@material-ui/core/GridList';
import GridListTile from '@material-ui/core/GridListTile';
import CodeCard from './CodeCard.js'

const useStyles = makeStyles(theme => ({
  gridList: {
    width: "100%",
    height: "70%",
    margin: 20
  }
}));

const data = [0, 1, 2, 3, 4, 5]

function compute_tile(tile){
  return (<GridListTile key={tile}>
            <CodeCard></CodeCard>
          </GridListTile>)
}

function compute_grid(data){
  return data.map(compute_tile)
}

export default function ObjectGrid(){
    const classes = useStyles();
    
    return (<div><GridList cellHeight={300} className={classes.gridList} cols={3}>
      {compute_grid(data)}
    </GridList>
    </div>)
}
