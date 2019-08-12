import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import GridList from '@material-ui/core/GridList';
import GridListTile from '@material-ui/core/GridListTile';
import ObjectCard from './ObjectCard.js'

const useStyles = makeStyles(theme => ({
  gridList: {
    width: "100%",
    height: "70%",
    marginLeft: 20
  }
}));

const data = [0, 1, 2, 3, 4, 5]

function compute_tile(tile){
  return (<GridListTile key={tile}>
            <ObjectCard></ObjectCard>
          </GridListTile>)
}

function compute_grid(data){
  return data.map(compute_tile)
}

export default function ObjectGrid(){
    const classes = useStyles();
    
    return (<GridList cellHeight={300} className={classes.gridList} cols={4} spacing={20}>
      {compute_grid(data)}
    </GridList>
    )
}
