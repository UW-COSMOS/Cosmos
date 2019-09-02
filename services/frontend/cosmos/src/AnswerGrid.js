import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import GridList from '@material-ui/core/GridList';
import GridListTile from '@material-ui/core/GridListTile';
import AnswerCard from './AnswerCard.js'

const useStyles = makeStyles(theme => ({
  root: {
    display: 'flex',
    justifyContent: 'center',
    overflow: 'hidden',
  },
  gridList: {
    width: "90%",
    height: 700,
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

  return (<GridListTile key={obj._id}>
            <AnswerCard object={obj} doi={doi} show={isNotTable}></AnswerCard>
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

export default function AnswerGrid(props){
    const classes = useStyles();
    
    return (<div><GridList cellHeight={700} className={classes.gridList} cols={2} spacing={20}>
      {compute_grid(props.objects, props.dois)}
    </GridList>
    </div>
    )
}
