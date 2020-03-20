"""Add JSON column for initial classes and confidences from detect

Revision ID: d3c66414bf63
Revises: 2804d3ebab05
Create Date: 2020-03-20 17:09:07.648755

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = 'd3c66414bf63'
down_revision = '2804d3ebab05'
branch_labels = None
depends_on = None


def upgrade():
    op.add_column('page_objects', 
        Column('init_cls_confidences', JSON))

