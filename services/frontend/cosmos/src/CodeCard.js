import React from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { makeStyles } from '@material-ui/core/styles';
import Card from '@material-ui/core/Card';
import CardHeader from '@material-ui/core/CardHeader';
import CardContent from '@material-ui/core/CardContent';
import IconButton from '@material-ui/core/IconButton';
import InsertChartIcon from '@material-ui/icons/InsertChart';


const useStyles = makeStyles(theme => ({
  card: {
    maxWidth: 1000,
    backgroundColor: "#f1f1f1"
  },
  highlighter: {
    margin: 20,
    backgroundColor: "#ffffff"
  }
}));

export default function CodeCard(props) {
  const classes = useStyles();

  return (
    <Card className={classes.card}>
      <CardHeader
        action={
          <IconButton aria-label="analyze" onClick={() => {props.handleClick(props.data.phrase)}}>
            <InsertChartIcon />
          </IconButton>
        }

        title={props.data.phrase}
        subheader={"Detected Entity"}
      />
      <CardContent>
        <SyntaxHighlighter language='fortran' showLineNumbers={true} startingLineNumber={props.data.line_number} customStyle={{margin: 20, backgroundColor: "#ffffff"}}>
          {props.data.line}
        </SyntaxHighlighter>
      </CardContent>
    </Card>
  );
}