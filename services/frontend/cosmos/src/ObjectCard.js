import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import clsx from 'clsx';
import Card from '@material-ui/core/Card';
import CardHeader from '@material-ui/core/CardHeader';
import CardMedia from '@material-ui/core/CardMedia';
import CardContent from '@material-ui/core/CardContent';
import CardActions from '@material-ui/core/CardActions';
import Collapse from '@material-ui/core/Collapse';
import Avatar from '@material-ui/core/Avatar';
import IconButton from '@material-ui/core/IconButton';
import Typography from '@material-ui/core/Typography';
import { red } from '@material-ui/core/colors';
import FavoriteIcon from '@material-ui/icons/Favorite';
import SendIcon from '@material-ui/icons/Send';
import ExpandMoreIcon from '@material-ui/icons/ExpandMore';
import MoreVertIcon from '@material-ui/icons/MoreVert';
import CardImage from './CardImage';
import Hidden from '@material-ui/core/Hidden';

const useStyles = makeStyles(theme => ({
  card: {
    maxWidth: 1000,
    backgroundColor: "#f1f1f1"
  },
  expand: {
    transform: 'rotate(0deg)',
    marginLeft: 'auto',
    transition: theme.transitions.create('transform', {
      duration: theme.transitions.duration.shortest,
    }),
  },
  expandOpen: {
    transform: 'rotate(180deg)',
  },
  avatar: {
    backgroundColor: red[500],
  },
  media: {
    height: 'auto',
    margin: 10,
  },
}));


export default function ObjectCard(props) {
  const classes = useStyles();
  const [expanded, setExpanded] = React.useState(false);

  function handleExpandClick() {
    setExpanded(!expanded);
  }
  var object = props.object
  var doi = props.doi


  return (
    <Card className={classes.card}>
      <CardHeader
        action={
          <IconButton aria-label="settings" >
            <MoreVertIcon />
          </IconButton>
        }
        title={doi.title}
        subheader={doi.url}
      />
      <CardMedia
        className={classes.media}
      ><CardImage bstring={object.bytes}></CardImage></CardMedia>
      <CardContent>
        <Typography variant="body2" color="textSecondary" component="p">
            {object.content}
        </Typography>
      </CardContent>
      <CardActions disableSpacing>
        <Hidden xlDown={props.show}>
        <IconButton aria-label="download">
          <SendIcon />
        </IconButton>
        </Hidden>
      </CardActions>
    </Card>
  );
}