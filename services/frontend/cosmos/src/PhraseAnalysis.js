import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import QAAnswer from './QAAnswer.js'

const useStyles = makeStyles(theme => ({
    root:{
        flexbox: 1
    },
    header:{
        marginBottom:20
    }
}))

export default function PhraseAnalysis(props){
    const classes = useStyles();
    return (<div>
        <Typography variant='h3' component='h3' className={classes.header}>
            {`${props.phrase} Analysis`}
        </Typography>
        <Typography variant='h5' component='h5'>
            {`What is ${props.phrase}?`}
        </Typography>
        <QAAnswer answer='PLACEHOLDER ANSWER' doi='https://example.com'></QAAnswer>

    </div>)

}