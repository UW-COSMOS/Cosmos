"""Adding table object

Revision ID: 2804d3ebab05
Revises: 7c8710da35ef
Create Date: 2020-03-18 21:13:45.809988

"""
from alembic import op
import sqlalchemy as sa
from sqlalchemy import Column, Integer, String, JSON, LargeBinary, ForeignKey, Boolean, Numeric, Text


# revision identifiers, used by Alembic.
revision = '2804d3ebab05'
down_revision = '7c8710da35ef'
branch_labels = None
depends_on = None


def upgrade():
    op.create_table(
        'tables',
        Column('id', Integer, primary_key=True),
        Column('page_object_id', Integer, ForeignKey('page_objects.id')),
        Column('df', LargeBinary(length=(2**32)-1)),
        Column('page_id', Integer, ForeignKey('pages.id'))
    )


def downgrade():
    pass
