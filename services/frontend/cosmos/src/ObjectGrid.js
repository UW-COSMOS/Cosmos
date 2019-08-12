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


function compute_tile(zip){
  var obj = zip[0]
  var doi = zip[1]
  if (typeof doi === "undefined") {
    return (<div></div>)
  }
  var isNotTable = true;
  if(obj.class == 'Table'){
    isNotTable = false;
  }

  return (<GridListTile key={obj.id}>
            <ObjectCard object={obj} doi={doi} show={isNotTable}></ObjectCard>
          </GridListTile>)
}

function compute_grid(objects, dois){
  console.log(dois)
  if (typeof(objects) === "undefined"){
    return (<div></div>)
  }
  var zipped = objects.map(function(o, i){
    for(var j = 0; j < dois.length; j++){
      let pdf_id = o.pdf_name.slice(0, -4)
      if(dois[j].pdf_id == pdf_id){
        return [o, dois[j]]
      }
    }
    return [o, dois[i]]
  })
  return zipped.map(compute_tile)
}

export default function ObjectGrid(props){
    const classes = useStyles();
    
    return (<GridList cellHeight={700} className={classes.gridList} cols={2} spacing={20}>
      {compute_grid(props.objects, props.dois)}
    </GridList>
    )
}
