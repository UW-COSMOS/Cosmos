import React from 'react';
import { makeStyles } from '@material-ui/core/styles';

const useStyles = makeStyles(theme => ({
  img: {
    width: 50,
    height: 50
  },
}));

export default function CardImage(props){
    const classes = useStyles()

    return (<img style={{width: 300, height:'auto', maxHeight: 400}} src={`data:image/png;base64,${props.bstring}`}></img>)
}