import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import Typography from '@material-ui/core/Typography';
import QAAnswer from './QAAnswer.js'
import RelatedTerms from './RelatedTerms.js'
import Histogram from './Histogram.js'
import ObjectGrid from './ObjectGrid.js'
import Grid from '@material-ui/core/Grid'

const useStyles = makeStyles(theme => ({
    root:{
        flexGrow: 1
    },
    header:{
        marginTop:20,
        marginBottom:20
    },
    objectGrid:{
        display: 'flex',
        justifyContent: 'center',
        overflow: 'hidden'
    }
}))

const DATA = [
    {x0: 0.0, x: 0.1, y: 4},
    {x0: 0.1, x: 0.2, y: 7},
    {x0: 0.3, x: 0.4, y: 9},
    {x0: 0.4, x: 0.5, y: 7},
    {x0: 0.5, x: 0.6, y: 7},
    {x0: 0.6, x: 0.7, y: 3},
    {x0: 0.7, x: 0.8, y: 2},
    {x0: 0.8, x: 0.9, y: 1},
    {x0: 0.9, x: 1.0, y: 2},
  ];
export default function PhraseAnalysis(props){
    const classes = useStyles();
    return (<Grid container justify='center' className={classes.root} spacing={2}>
        <Grid item xs={10}>
        <Typography variant='h3' component='h3' className={classes.header}>
            {`${props.phrase} Analysis`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <Typography variant='h4' component='h4'>
            {`What is ${props.phrase}?`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <QAAnswer answer={props.answer} doi={props.answerDOI}></QAAnswer>
        </Grid>

        <Grid item xs={10}>
        <RelatedTerms relatedTerms={props.relatedTerms} hideProgress={false}></RelatedTerms>
        </Grid>

        <Grid item xs={10}>
        <Typography variant="h4" component="h4">
          Extracted empirical distribution over {props.phrase}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <Histogram data={DATA} xdomain={[0,1]} ydomain={[0,10]}></Histogram>
        </Grid>

        <Grid item xs={10}>
        <Typography variant='h4' component='h4' className={classes.header}>
            {`Tabular data for ${props.phrase}`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <ObjectGrid objects={props.tableObjects} dois={props.tableDOIs} className={classes.objectGrid}></ObjectGrid>
        </Grid>

        <Grid item xs={10}>
        <Typography variant='h4' component='h4' className={classes.header}>
            {`Related Equations`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <ObjectGrid objects={props.equationObjects} dois={props.equationDOIs} className={classes.objectGrid}></ObjectGrid>
        </Grid>

        <Grid item xs={10}>
        <Typography variant='h4' component='h4' className={classes.header}>
            {`Related Figures`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <ObjectGrid objects={props.figureObjects} dois={props.figureDOIs} className={classes.objectGrid}></ObjectGrid>
        </Grid>

        <Grid item xs={10}>
        <Typography variant='h4' component='h4' className={classes.header}>
            {`Related Text Blobs`}
        </Typography>
        </Grid>

        <Grid item xs={10}>
        <ObjectGrid objects={props.textObjects} dois={props.textDOIs} className={classes.objectGrid}></ObjectGrid>
        </Grid>
        </Grid>)


}